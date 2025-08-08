#!/bin/bash

# Grid Engine options (lines prefixed with #$)
#$ -cwd                         # Run from current working directory
#$ -l h_vmem=16G                 # Request 8GB memory per task (adjust as needed)
#$ -l h_rt=10:00:00             # Request 2 hours runtime per task (adjust as needed)
#$ -N ea_sweep                  # Job name
#$ -o logs/                     # Output directory (create logs/ directory first)
#$ -e logs/                     # Error directory

# Print job information
echo "========================================="
echo "Job started on: $(date)"
echo "Job ID: $JOB_ID"
echo "Task ID: $SGE_TASK_ID"
echo "Hostname: $(hostname)"
echo "Working directory: $(pwd)"
echo "========================================="

# Set up environment
# Initialise the environment modules
. /etc/profile.d/modules.sh

# Load java
module load java/jdk-22.0.1

# Check command line arguments
if [ $# -lt 4 ]; then
    echo "Error: Missing required arguments"
    echo "Usage: qsub -t 1-MAX_TASKS run_array_job.sh CSV_FILE MAX_TASKS JOB_NUMBER TOTAL_JOBS [MODEL_FILE]"
    echo ""
    echo "Arguments:"
    echo "  CSV_FILE: Path to parameter combinations CSV file"
    echo "  MAX_TASKS: Maximum tasks per job (e.g., 10000)"
    echo "  JOB_NUMBER: Which job this is (1 to TOTAL_JOBS)"
    echo "  TOTAL_JOBS: Total number of jobs the CSV is split across"
    echo "  MODEL_FILE: NetLogo model file (optional, defaults to toy_model_4.6.1.nlogo)"
    echo ""
    echo "Example usage:"
    echo "  # For 1,000,000 combinations split across 10 jobs of 10,000 tasks each:"
    echo "  qsub -t 1-10000 run_array_job.sh parameter_combinations.csv 10000 1 10"
    echo "  qsub -t 1-10000 run_array_job.sh parameter_combinations.csv 10000 2 10"
    echo "  ..."
    echo "  qsub -t 1-10000 run_array_job.sh parameter_combinations.csv 10000 10 10"
    exit 1
fi

# Get parameters
CSV_FILE="$1"
MAX_TASKS="$2"
JOB_NUMBER="$3"
TOTAL_JOBS="$4"
MODEL_FILE="${5:-toy_model_4.6.1.nlogo}"

# Validate numeric parameters
if ! [[ "$MAX_TASKS" =~ ^[0-9]+$ ]] || [ "$MAX_TASKS" -le 0 ]; then
    echo "Error: MAX_TASKS must be a positive integer, got: $MAX_TASKS"
    exit 1
fi

if ! [[ "$JOB_NUMBER" =~ ^[0-9]+$ ]] || [ "$JOB_NUMBER" -le 0 ]; then
    echo "Error: JOB_NUMBER must be a positive integer, got: $JOB_NUMBER"
    exit 1
fi

if ! [[ "$TOTAL_JOBS" =~ ^[0-9]+$ ]] || [ "$TOTAL_JOBS" -le 0 ]; then
    echo "Error: TOTAL_JOBS must be a positive integer, got: $TOTAL_JOBS"
    exit 1
fi

if [ "$JOB_NUMBER" -gt "$TOTAL_JOBS" ]; then
    echo "Error: JOB_NUMBER ($JOB_NUMBER) cannot exceed TOTAL_JOBS ($TOTAL_JOBS)"
    exit 1
fi

if [ "$SGE_TASK_ID" -gt "$MAX_TASKS" ]; then
    echo "Error: Task ID ($SGE_TASK_ID) exceeds MAX_TASKS ($MAX_TASKS)"
    exit 1
fi

# Validate input files exist
if [ ! -f "$CSV_FILE" ]; then
    echo "Error: Parameter CSV file not found: $CSV_FILE"
    exit 1
fi

if [ ! -f "$MODEL_FILE" ]; then
    echo "Error: NetLogo model file not found: $MODEL_FILE"
    exit 1
fi

# Get absolute paths for the files
CSV_ABSOLUTE=$(readlink -f "$CSV_FILE")
MODEL_ABSOLUTE=$(readlink -f "$MODEL_FILE")

# Print task information
echo "Task $SGE_TASK_ID of Job $JOB_NUMBER/$TOTAL_JOBS starting..."
echo "Max tasks per job: $MAX_TASKS"
echo "Using CSV file: $CSV_ABSOLUTE"
echo "Using model file: $MODEL_ABSOLUTE"

# Create output directory for this task if it doesn't exist
TASK_DIR="output_job${JOB_NUMBER}_task${SGE_TASK_ID}"
mkdir -p "$TASK_DIR"
cd "$TASK_DIR"

# Run the Java program with all required parameters
echo "Running simulation for task $SGE_TASK_ID (Job $JOB_NUMBER/$TOTAL_JOBS)..."
java -cp ../app/netlogo-6.4.0.jar:.. -Dnetlogo.extensions.dir=.. -Xmx12g ModelSimArrayBatch \
    "$SGE_TASK_ID" "$CSV_ABSOLUTE" "$MAX_TASKS" "$JOB_NUMBER" "$TOTAL_JOBS" "$MODEL_ABSOLUTE"

# Check if simulation completed successfully
if [ $? -eq 0 ]; then
    echo "Task $SGE_TASK_ID (Job $JOB_NUMBER/$TOTAL_JOBS) completed successfully"
    
    # Move and compress immediately
    for file in *.csv; do
        if [ -f "$file" ]; then
            job_prefixed_file="job${JOB_NUMBER}_${file}"
            # Compress while moving
            gzip -c "$file" > "../results/${job_prefixed_file}.gz"
        fi  
    done
    
    # Clean up task directory
    cd ..
    rm -rf "$TASK_DIR"
    
else
    echo "Error: Task $SGE_TASK_ID (Job $JOB_NUMBER/$TOTAL_JOBS) failed"
    cd ..
    exit 1
fi

echo "========================================="
echo "Job finished on: $(date)"
echo "========================================="