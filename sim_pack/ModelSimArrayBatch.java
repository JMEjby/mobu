import java.io.*;
import java.nio.file.*;
import java.util.*;
import org.nlogo.headless.HeadlessWorkspace;

public class ModelSimArrayBatch {
    
    public static void runHeadlessSimulation(int taskId, String csvFilename, String modelPath, 
                                           int maxTasks, int jobNumber, int totalJobs) {
        HeadlessWorkspace workspace = null;
        
        try {
            // Create headless workspace
            workspace = HeadlessWorkspace.newInstance();
            
            // Open the NetLogo model
            workspace.open(modelPath);
            
            // Call the headless sweep function with all parameters
            String outputDir = String.format("./output_job%d_task%d", jobNumber, taskId);

            String command = String.format("headless-sweep %d \"%s\" %d %d %d \"%s\"",
                               taskId, csvFilename, maxTasks, jobNumber, totalJobs, outputDir);

            workspace.command(command);
            
            System.out.println("Task " + taskId + " (Job " + jobNumber + "/" + totalJobs + ") completed successfully");
            
        } catch (Exception e) {
            System.err.println("Error running task " + taskId + " (Job " + jobNumber + "/" + totalJobs + "): " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        } finally {
            // Clean up workspace
            if (workspace != null) {
                try {
                    workspace.dispose();
                } catch (Exception e) {
                    System.err.println("Error disposing workspace: " + e.getMessage());
                }
            }
        }
    }
    
    public static void main(String[] args) {
        // Check command line arguments
        if (args.length < 5) {
            System.err.println("Usage: java ModelSimArrayBatch <task_id> <csv_filename> <max_tasks> <job_number> <total_jobs> [model_path]");
            System.err.println("  task_id: Integer task ID from SGE_TASK_ID (1 to max_tasks)");
            System.err.println("  csv_filename: Path to parameter combinations CSV file");
            System.err.println("  max_tasks: Maximum tasks per job (e.g., 10000)");
            System.err.println("  job_number: Which job this is (1 to total_jobs)");
            System.err.println("  total_jobs: Total number of jobs the CSV is split across");
            System.err.println("  model_path: Path to NetLogo model file (optional, defaults to toy_model_4.6.1.nlogo)");
            System.err.println();
            System.err.println("Example: java ModelSimArrayBatch 1 params.csv 10000 1 10");
            System.err.println("  This runs task 1 of job 1 out of 10 total jobs, with max 10000 tasks per job");
            System.exit(1);
        }
        
        try {
            // Parse command line arguments
            int taskId = Integer.parseInt(args[0]);
            String csvFilename = args[1];
            int maxTasks = Integer.parseInt(args[2]);
            int jobNumber = Integer.parseInt(args[3]);
            int totalJobs = Integer.parseInt(args[4]);
            String modelPath = args.length > 5 ? args[5] : "toy_model_4.6.1.nlogo";
            
            // Validate inputs
            if (taskId <= 0) {
                System.err.println("Error: Task ID must be a positive integer, got: " + taskId);
                System.exit(1);
            }
            
            if (maxTasks <= 0) {
                System.err.println("Error: Max tasks must be a positive integer, got: " + maxTasks);
                System.exit(1);
            }
            
            if (jobNumber <= 0 || jobNumber > totalJobs) {
                System.err.println("Error: Job number must be between 1 and " + totalJobs + ", got: " + jobNumber);
                System.exit(1);
            }
            
            if (totalJobs <= 0) {
                System.err.println("Error: Total jobs must be a positive integer, got: " + totalJobs);
                System.exit(1);
            }
            
            if (taskId > maxTasks) {
                System.err.println("Error: Task ID (" + taskId + ") cannot exceed max tasks (" + maxTasks + ")");
                System.exit(1);
            }
            
            // Check if CSV file exists
            File csvFile = new File(csvFilename);
            if (!csvFile.exists()) {
                System.err.println("Error: Parameter CSV file not found: " + csvFilename);
                System.exit(1);
            }
            
            // Check if model file exists
            File modelFile = new File(modelPath);
            if (!modelFile.exists()) {
                System.err.println("Error: NetLogo model file not found: " + modelPath);
                System.exit(1);
            }
            
            System.out.println("Starting Task " + taskId + " of Job " + jobNumber + "/" + totalJobs);
            System.out.println("Max tasks per job: " + maxTasks);
            System.out.println("Using parameter file: " + csvFilename);
            System.out.println("Using model file: " + modelPath);
            
            // Run the simulation
            runHeadlessSimulation(taskId, csvFilename, modelPath, maxTasks, jobNumber, totalJobs);
            
        } catch (NumberFormatException e) {
            System.err.println("Error: Invalid number format in arguments");
            System.err.println("Task ID, max tasks, job number, and total jobs must all be integers");
            System.exit(1);
        } catch (Exception e) {
            System.err.println("Unexpected error: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}