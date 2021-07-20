\name{tmplate-package}
\alias{tmplate-package}
\alias{tmplate}
\title{
'Code Generation Based on Templates'
}
\description{
Define general templates with tags that can be replaced by content depending on arguments and objects to modify the final output of the document.
}
\details{
Function \code{translate} receives a template (as a character vector with the lines of a template file which contains tags either for content or R code) a list of argument variables that will be used to translate those tags to content. The evaluation is performed in an environment defined by the user and replaces its output in the template returning a character vector with the lines of the resulting template.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~ 
\examples{
library(tmplate)
#
### use the following template file
T <- readLines(system.file("examples/template.txt", package = "tmplate"))
#
### translate template content using the following values as input arguments
TT <- translate(
SHELL_CALL='#!/bin/bash',
SLURM_SBATCH=ifelse(.Platform$OS.type=="unix", 
ifelse(system("clu=$(sinfo --version 2>&1) || clu=`echo -1`; echo $clu", intern = TRUE)=="-1",
'<:MISS:>','#SBATCH '),'<:MISS:>'),
SLURM_PARTITION='<:SLURM_SBATCH:>--partition=defq',
SLURM_ASK_NODES=2,
SLURM_NODES='<:SLURM_SBATCH:>--nodes=<:SLURM_ASK_NODES:>',
SLURM_ASK_TASKS=4,
SLURM_TASKS='<:SLURM_SBATCH:>--ntasks-per-node=<:SLURM_ASK_TASKS:>',
SLURM_MEMORY='<:SLURM_SBATCH:>--memory=2gb',
SLURM_TIME='<:SLURM_SBATCH:>--time=1:00:00',
SLURM_ARRAY="<:MISS:>",
MODULES_LOAD='module load module/for/openmpi module/for/R',
WORKDIR=ifelse('<:SLURM_SBATCH:>'!='#SBATCH','# no slurm machine','cd ${SLURM_SUBMIT_DIR}'),
TASK="<:MISS:>",
PASS_TASK="<:MISS:>",
PASS_TASK_VAR="<:MISS:>",
MPI_N="<:MISS:>",
MPI_ASK_N='<r@ <:SLURM_ASK_NODES:> * <:SLURM_ASK_TASKS:> @>',
R_HOME=R.home("bin"),
R_OPTIONS='--no-save --no-restore',
R_FILE_INPUT='script.R',
R_ARGS='',
R_FILE_OUTPUT='output.Rout',
MPIRUN='mpirun --mca mpi_warn_on_fork 0 -n <:MPI_ASK_N:> / 
<:R_HOME:>/Rscript <:R_OPTIONS:> "<:R_FILE_INPUT:>" /
<r@ ifelse(!any(grepl("^<:MISS:>$","<:SLURM_ARRAY:>")),"<:PASS_TASK_VAR:>","") @> /
<:R_ARGS:> > <:R_FILE_OUTPUT:>',
MESSAGE_CLOSE='echo "Job submitted on $(date)."',
drop = TRUE,
default = "MISS",
template = T
)
#
### run TT to display resulting output (or print using 'cat' with newline as separator)
TT
#
### Or See instructions in the following file:
### (example present in the vignette)
source(system.file("examples/example.R", package = "tmplate"))
}

