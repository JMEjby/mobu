#!/bin/bash

# Script to submit multiple array jobs for large parameter sweeps
# This script splits a large parameter sweep into multiple manageable jobs
# and automatically handles data staging to DataStore

# Default configuration (can be overridden by command line arguments)
DEFAULT_CSV_FILE="parameter_combinations.csv"
DEFAULT_MODEL_FILE="toy_model_4.6.1.nlogo"
DEFAULT_MAX_TASKS_PER_JOB=10000
DEFAULT_DATASTORE_PATH="/exports/chss/datastore/ppls/users/s1917169"
DEFAULT_PROJECT_NAME="ea_results"

# Function to display help
show_help() {
    echo "Usage: $0 [CSV_FILE] [TOTAL_JOBS] [MAX_TASKS_PER_JOB] [MODEL_FILE] [DATASTORE_PATH] [PROJECT_NAME]"
    echo ""
    echo "Submit multiple array jobs for EA parameter sweep with automatic DataStore staging"
    echo ""
    echo "Arguments:"
    echo "  CSV_FILE          Path to parameter combinations CSV file"
    echo "                    (default: $DEFAULT_CSV_FILE)"
    echo "  TOTAL_JOBS        Number of jobs to split the work across"
    echo "                    (required if CSV_FILE is provided)"
    echo "  MAX_TASKS_PER_JOB Maximum tasks per job (default: $DEFAULT_MAX_TASKS_PER_JOB)"
    echo "  MODEL_FILE        NetLogo model file (default: $DEFAULT_MODEL_FILE)"
    echo "  DATASTORE_PATH    Path to DataStore space (default: $DEFAULT_DATASTORE_PATH)"
    echo "                    (if provided, enables automatic staging)"
    echo "  PROJECT_NAME      Project folder name in DataStore (default: $DEFAULT_PROJECT_NAME)"
    echo ""
    echo "Examples:"
    echo "  $0 params.csv 10                                    # 10 jobs, default staging"
    echo "  $0 params.csv 10 5000                              # 10 jobs, 5000 tasks each"
    echo "  $0 params.csv 10 5000 model.nlogo \\               # With custom model"
    echo "    /exports/chss/datastore/ppls/users/s1917169 my_study     # And custom project name"
    echo ""
    echo "DataStore staging workflow:"
    echo "  1. Runs compute jobs (ea_sweep_job1, ea_sweep_job2, ...)"
    echo "  2. Stages each job's results (stage_job1 depends on ea_sweep_job1, ...)"
    echo "  3. Combines all aggregated data and stages it (combine_and_stage depends on all stage_jobs)"
    echo ""
    echo "Output folder structure in DataStore:"
    echo "  $DEFAULT_DATASTORE_PATH/$DEFAULT_PROJECT_NAME/"
    echo "  +-- aggregated/              # Individual aggregated files"
    echo "  +-- agent-level/             # Agent-level data files"
    echo "  +-- alignment-matrices/      # Alignment matrix files"
    echo "  +-- misalignment-matrices/   # Misalignment matrix files"

    echo ""
}

# Parse command line arguments
if [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
    show_help
    exit 0
fi

CSV_FILE="${1:-$DEFAULT_CSV_FILE}"
TOTAL_JOBS="$2"
MAX_TASKS_PER_JOB="${3:-$DEFAULT_MAX_TASKS_PER_JOB}"
MODEL_FILE="${4:-$DEFAULT_MODEL_FILE}"
DATASTORE_PATH="${5:-$DEFAULT_DATASTORE_PATH}"
PROJECT_NAME="${6:-$DEFAULT_PROJECT_NAME}"

# Validate required arguments
if [ -z "$TOTAL_JOBS" ]; then
    echo "Error: TOTAL_JOBS is required when CSV_FILE is specified"
    echo ""
    show_help
    exit 1
fi

# DataStore staging is always enabled with default path
ENABLE_STAGING=true
echo "DataStore staging enabled"
echo "  DataStore path: $DATASTORE_PATH"
echo "  Project name: $PROJECT_NAME"

# Validate numeric inputs
if ! [[ "$TOTAL_JOBS" =~ ^[0-9]+$ ]] || [ "$TOTAL_JOBS" -le 0 ]; then
    echo "Error: TOTAL_JOBS must be a positive integer, got: $TOTAL_JOBS"
    exit 1
fi

if ! [[ "$MAX_TASKS_PER_JOB" =~ ^[0-9]+$ ]] || [ "$MAX_TASKS_PER_JOB" -le 0 ]; then
    echo "Error: MAX_TASKS_PER_JOB must be a positive integer, got: $MAX_TASKS_PER_JOB"
    exit 1
fi

# Validate input files
if [ ! -f "$CSV_FILE" ]; then
    echo "Error: Parameter CSV file not found: $CSV_FILE"
    exit 1
fi

if [ ! -f "$MODEL_FILE" ]; then
    echo "Error: NetLogo model file not found: $MODEL_FILE"
    exit 1
fi

# Validate staging requirements
if [ ! -f "stage_job_results.sh" ]; then
    echo "Error: Staging script not found: stage_job_results.sh"
    echo "This script is required for DataStore staging"
    exit 1
fi

if [ ! -f "combine_and_stage.sh" ]; then
    echo "Error: Combination script not found: combine_and_stage.sh"
    echo "This script is required for DataStore staging"
    exit 1
fi

# Create necessary directories
mkdir -p logs results

# Calculate total combinations (subtract 1 for header)
TOTAL_COMBINATIONS=$(( $(wc -l < "$CSV_FILE") - 1 ))
COMBINATIONS_PER_JOB=$(( TOTAL_COMBINATIONS / TOTAL_JOBS ))
TOTAL_TASKS=$(( TOTAL_JOBS * MAX_TASKS_PER_JOB ))

echo "========================================="
echo "EA Parameter Sweep Submission"
echo "========================================="
echo "CSV file: $CSV_FILE"
echo "Model file: $MODEL_FILE"
echo "Total parameter combinations: $TOTAL_COMBINATIONS"
echo "Total jobs: $TOTAL_JOBS"
echo "Max tasks per job: $MAX_TASKS_PER_JOB"
echo "Total tasks across all jobs: $TOTAL_TASKS"
echo "Approximate combinations per job: $COMBINATIONS_PER_JOB"
echo "Approximate combinations per task: $(( COMBINATIONS_PER_JOB / MAX_TASKS_PER_JOB ))"

echo ""
echo "DataStore Staging Configuration:"
echo "  DataStore path: $DATASTORE_PATH"
echo "  Project name: $PROJECT_NAME"
echo "  Individual job staging: Enabled"
echo "  Combined aggregated data: Enabled"
echo "  Output organization:"
echo "    - Aggregated files â†’ aggregated/"
echo "    - Agent-level files â†’ agent-level/"
echo "    - Alignment matrices â†’ alignment-matrices/"
echo "    - Misalignment matrices â†’ misalignment-matrices/"

echo ""

# Confirm before submission
TOTAL_JOBS_TO_SUBMIT=$((TOTAL_JOBS * 2 + 1))  # compute + staging + combine
read -p "Submit $TOTAL_JOBS compute jobs + $TOTAL_JOBS staging jobs + 1 combination job = $TOTAL_JOBS_TO_SUBMIT total jobs? (y/N): " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Cancelled."
    exit 0
fi

echo "Submitting jobs..."
echo ""

# Submit compute jobs
echo "=== Submitting Compute Jobs ==="
COMPUTE_JOB_IDS=()
for JOB_NUMBER in $(seq 1 $TOTAL_JOBS); do
    echo "Submitting compute job $JOB_NUMBER/$TOTAL_JOBS..."
    
    # Submit the array job with unique name
    JOB_NAME="ea_sweep_job$JOB_NUMBER"
    SUBMIT_OUTPUT=$(qsub -N "$JOB_NAME" -t 1-$MAX_TASKS_PER_JOB run_array_job.sh \
        "$CSV_FILE" "$MAX_TASKS_PER_JOB" "$JOB_NUMBER" "$TOTAL_JOBS" "$MODEL_FILE" 2>&1)
    
    if [ $? -eq 0 ]; then
        # Extract job ID from qsub output
        JOB_ID=$(echo "$SUBMIT_OUTPUT" | grep -o '[0-9]\+' | head -1)
        echo "  âœ“ Submitted successfully: $JOB_NAME (Job ID $JOB_ID)"
        COMPUTE_JOB_IDS+=("$JOB_ID")
    else
        echo "  âœ— Error submitting compute job $JOB_NUMBER:"
        echo "    $SUBMIT_OUTPUT"
        exit 1
    fi
    
    sleep 0.2
done

# Submit staging jobs
STAGING_JOB_IDS=()
echo ""
echo "=== Submitting Individual Staging Jobs ==="

for JOB_NUMBER in $(seq 1 $TOTAL_JOBS); do
    echo "Submitting staging job $JOB_NUMBER/$TOTAL_JOBS..."
    
    # Submit staging job that depends on corresponding compute job
    COMPUTE_JOB_NAME="ea_sweep_job$JOB_NUMBER"
    STAGING_JOB_NAME="stage_job$JOB_NUMBER"
    
    SUBMIT_OUTPUT=$(qsub -N "$STAGING_JOB_NAME" -hold_jid "$COMPUTE_JOB_NAME" \
        stage_job_results.sh "$JOB_NUMBER" "$DATASTORE_PATH" "$PROJECT_NAME" 2>&1)
    
    if [ $? -eq 0 ]; then
        JOB_ID=$(echo "$SUBMIT_OUTPUT" | grep -o '[0-9]\+' | head -1)
        echo "  âœ“ Submitted successfully: $STAGING_JOB_NAME (Job ID $JOB_ID, depends on $COMPUTE_JOB_NAME)"
        STAGING_JOB_IDS+=("$JOB_ID")
    else
        echo "  âœ— Error submitting staging job $JOB_NUMBER:"
        echo "    $SUBMIT_OUTPUT"
        exit 1
    fi
    
    sleep 0.2
done

echo ""
echo "=== Submitting Combined Aggregated Data Job ==="

# Submit combination job that depends on all staging jobs
STAGING_DEPENDENCY="stage_job*"  # Wildcard dependency on all staging jobs
COMBINE_JOB_NAME="combine_and_stage"

echo "Submitting combined aggregated data job..."
SUBMIT_OUTPUT=$(qsub -N "$COMBINE_JOB_NAME" -hold_jid "$STAGING_DEPENDENCY" \
    combine_and_stage.sh "$DATASTORE_PATH" "$PROJECT_NAME" "$TOTAL_JOBS" 2>&1)

if [ $? -eq 0 ]; then
    COMBINE_JOB_ID=$(echo "$SUBMIT_OUTPUT" | grep -o '[0-9]\+' | head -1)
    echo "  âœ“ Submitted successfully: $COMBINE_JOB_NAME (Job ID $COMBINE_JOB_ID, depends on all stage_job*)"
else
    echo "  âœ— Error submitting combination job:"
    echo "    $SUBMIT_OUTPUT"
    exit 1
fi

echo ""
echo "========================================="
echo "All Jobs Submitted Successfully!"
echo "========================================="
echo "Compute jobs: ${#COMPUTE_JOB_IDS[@]} (IDs: ${COMPUTE_JOB_IDS[*]})"
echo "Staging jobs: ${#STAGING_JOB_IDS[@]} (IDs: ${STAGING_JOB_IDS[*]})"
echo "Combination job: $COMBINE_JOB_ID"
echo ""
echo "Workflow:"
echo "  1. Compute jobs run simulations and save results to Eddie"
echo "  2. Staging jobs organize and copy results from Eddie to DataStore"
echo "  3. Combination job merges all aggregated data and stages final file"

echo ""
echo "Monitor commands:"
echo "  qstat                    # Check all job status"
echo "  qstat -u \$USER          # Check your jobs only"
echo "  qdel JOB_ID             # Cancel a specific job"

echo ""
echo "DataStore output locations:"
echo "  Project directory: $DATASTORE_PATH/$PROJECT_NAME/"
echo "  Aggregated files: $DATASTORE_PATH/$PROJECT_NAME/aggregated/"
echo "  Agent-level files: $DATASTORE_PATH/$PROJECT_NAME/agent-level/"
echo "  Alignment matrices: $DATASTORE_PATH/$PROJECT_NAME/alignment-matrices/"
echo "  Misalignment matrices: $DATASTORE_PATH/$PROJECT_NAME/misalignment-matrices/"
echo "  Combined data: $DATASTORE_PATH/$PROJECT_NAME/combined_aggregated_data_*.csv"

echo ""
echo "Expected output files per parameter combination:"
echo "  - job[N]_agent_level_combo[ID]_run[R]_*.csv â†’ agent-level/"
echo "  - job[N]_aggregated_combo[ID]_*.csv â†’ aggregated/"
echo "  - job[N]_alignment_matrix_combo[ID]_run[R]_*.csv â†’ alignment-matrices/"
echo "  - job[N]_misalignment_matrix_combo[ID]_run[R]_*.csv â†’ misalignment-matrices/"

echo ""
echo "DataStore-specific files:"
echo "  - combined_aggregated_data_[TIMESTAMP].csv (all aggregated data combined)"
echo "  - combined_aggregated_data_[TIMESTAMP].csv.gz (compressed version)"
echo "  - job[N]_staging_manifest.txt (staging logs per job)"
echo "  - combined_aggregated_manifest_[TIMESTAMP].txt (combination log)"

echo ""
echo "Estimated completion time depends on parameter complexity and cluster load."