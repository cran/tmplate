###########################
## examples in vignettes ##
###########################
#library(tmplate)

# one line definition
T <- '<:SHELL_CALL:>
<:SLURM_PARTITION:>
<:SLURM_NODES:>
<:SLURM_TASKS:>
<:SLURM_MEMORY:>
<:SLURM_TIME:>
@r if(!any(grepl("^<:MISS:>$","<:SLURM_ARRAY:>"))) paste("<:SLURM_ARRAY:>")

@r # R chunk (only assignation)
@r if(.Platform$OS.type=="unix"){
@r     is_mod <- system("mod=$(module --dumpversion 2>&1) || mod=`echo -1`; echo $mod",intern = TRUE)
@r } else {
@r     is_mod <- "-1"
@r }

@r # R chunk (printing)
@r ifelse(is_mod=="-1", "# module environment not found", paste("<:MODULES_LOAD:>"))

<:WORKDIR:>

@r if(!any(grepl("^<:MISS:>$","<:SLURM_ARRAY:>"))) c("<:TASK:>","<:PASS_TASK:>") else paste("<:MISS:>")

<:MPI_N:>

<:MPIRUN:>

<:MESSAGE_CLOSE:>
'
# using input file
T <- readLines(system.file("examples/template.txt", package = "tmplate"))
# translate content
TT <- tmplate::translate(
    SHELL_CALL='#!/bin/bash',
    SLURM_SBATCH=ifelse(.Platform$OS.type=="unix", ifelse(system("clu=$(sinfo --version 2>&1) || clu=`echo -1`; echo $clu",intern = TRUE)=="-1", '<:MISS:>', '#SBATCH '), '<:MISS:>'),
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
    MPIRUN='mpirun --mca mpi_warn_on_fork 0 -n <:MPI_ASK_N:> <:R_HOME:>/Rscript <:R_OPTIONS:> /
    "<:R_FILE_INPUT:>" <r@ ifelse(!any(grepl("^<:NULL:>$","<:SLURM_ARRAY:>")),"<:PASS_TASK_VAR:>","") @> /
    <:R_ARGS:> > <:R_FILE_OUTPUT:>',
    MESSAGE_CLOSE='echo "Job submitted on $(date +%F) at $(date +%T)."',
    drop = TRUE,
    default = "MISS",
    template = T
)
# display result output
cat(TT, sep="\n")
