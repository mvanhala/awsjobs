# awsjobs

## Installation

```
devtools::install_github("mvanhala/awsjobs")
```

## Overview

This package is designed to help you run R scripts on EC2 worker instances.

The main function a user will generally invoke is `run_job_wrapper()`. One might
invoke like the following:

```
run_job_wrapper(
  script = "/efs/project/model_metrics.Rmd", 
  memory = 4, 
  cpu = 2,
  spot = TRUE,
  log_file = "/efs/project/metrics.log"
)
```

In this call, an EC2 worker instance will be launched. Using 
[Amazon SSM](https://docs.aws.amazon.com/systems-manager/latest/userguide/what-is-systems-manager.html) 
the R Markdown document `/efs/project/model_metrics.Rmd` will be executed on the launched instance
by sending a [basic shell script command via SSM](https://docs.aws.amazon.com/systems-manager/latest/userguide/walkthrough-cli.html#walkthrough-cli-example-1).

The user-provided specification of the minimum memory (in GB) and CPU cores for the EC2 instance will be
used to determine what instance type to launch. The `spot` parameter specifies whether or not to launch a spot instance.
Spot instances are fully supported by `awsjobs`, enabling users to take full advantage of the deep discounts (often 70%+)
of spot instances compared to on-demand pricing.

When a user invokes `run_job_wrapper`, the launching of the EC2 worker instance and execution of the
R script or R Markdown document are done via a child process launched via `rstudioapi::jobRunScript()`.
As a result, the R session from which the job is launched is not blocked. Hence the user can start
multiple jobs in succession to run simultaneously, then return back to the parent R process while those jobs
execute.

Information about the running job is printed in the RStudio Jobs pane. Note that the user should *not* use
the stop button to stop a running job. Doing this terminates the child process from which the EC2 instance
is launched, but leaves the running EC2 instance severed/orphaned, so it will keep running without any connection 
to an R process. Because the instance has a maximum lifetime set when launched, the instance won't keep running forever, and
will shutdown when the maximum is reached (or the ordinary execution on the instances finishes).
Nonetheless, the user will instead want to call [stop_job()], providing the job id
printed in the RStudio Jobs pane to send an interrupt to the child R process which will
trigger the termination of the running EC2 worker instance.

A couple of other functions of interest include `list_r_job_instances()`, 
which will return a data frame of all the
currently running R worker jobs, and `stop_instances()`, which allows the user to stop one or more
running EC2 instances by instance id.

## Assumptions and setup

Launching/terminating EC2 worker instances and sending SSM commands 
is performed using the [`paws`](https://github.com/paws-r/paws) package,
AWS SDK for R.

It is assumed that that the R scripts or R Markdown documents to execute are located on 
[AWS EFS](https://aws.amazon.com/efs/) storage, and that there is access to this from 
both the parent environment from which worker instances are launched, as well as on the
worker instances.

This means that the parent environment from which jobs are executed should be an RStudio
Server environment on an AWS EC2 instance to which an EFS file system has been attached.
This EFS file system will get attached to the worker instance after the worker instance
is launched.

In order to do this, the EFS file system needs to have an associated security group which
allows inbound traffic on port 2049. Similarly, the RStudio Server instance needs to 
allow traffic on port 2049, as do the EC2 worker instances, in order to allow communication
with the EFS storage.

The launched EC2 worker instances should have an IAM profile associated with them. This IAM
profile needs to have permissions to enable the instances to send/receive communication via 
SSM.

In order to successfully run a job, EC2, SSM, and IAM permissions are required. These should
either be granted through IAM user credentials (which the user can set as the 
environment variables `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY`, which will be picked up
by `paws` credential detection), or by being granted to an IAM role associated with the 
RStudio Server instance.

An example of a script for creating an AMI to use with worker instances is in the
[mvanhala/illustrations repo](https://github.com/mvanhala/illustrations/tree/master/code/aws_worker_jobs). 
The R script programmatically uses the `paws` SDK
to launch an EC2 instance, execute a shell script installing R, various system libraries, and
a collection of R packages, and create and tag an AMI for subsequent use in launching EC2 
worker instances. One can easily extend this to place R packages within the EFS storage and
setting a few environment variables, 
as well as using [renv](https://github.com/rstudio/renv) for package management, 
thereby enabling tighter integration of the R package environment between the development
environment on RStudio and the execution environment on the EC2 worker instances.

## Future enhancements

As opposed to relying on files being stored in attached EFS storage accessible to both
the parent environment from which instances are launched and the worker instances themselves,
one could construct a setup in which data and scripts are stored in S3, or passed to 
worker instances via SSH or EC2 instance user data.


