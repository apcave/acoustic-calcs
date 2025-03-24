# Composite Acoustic Simulator

This repository contains the code for server side simulator for the [Composite Acoustic](https://sound-wave.dev) web application hosted in the link.

It is a back-end [Djang](https://www.djangoproject.com/) REST API managing Fortran code that does the physics calculations. The key technologies and uses are;

- Python is a good choice for working on mixed language projects. It has build in features to provide for compiling Fortran, C and C++ to share libraries that can be loaded as Python modules. It also runs and builds in similar way that is independent of the OS processor used. For engineering, mathematics and AI there are many libraries that can be used to rapidly add rich feature to the API.

- Fortran and numpy is a high performance natural choice for application heavy in mathematics that use matrix, vectors and complex numbers. It also provides for enhance floating point precision. There is much legacy code in Fortran that can be utilized in modern Python applications.

- [Django](https://www.djangoproject.com/) provides RDS connectivity, authentication with JWT and REST endpoint code for HTTP commands. It have data views that serialize into a choosen RBD and has commercial grade performance in a small independently executed package. It also provide a web administration interface to manage your data.

- [PostgreSQL](https://www.postgresql.org) provide the non-model document related persistent storage. Having mixing SQL and NonSQL provides the benefits of relationship storage data and well as the hierarchies and speed associated with NonSQL.

- [drf-spectacular](https://drf-spectacular.readthedocs.io/en/latest/) enables a self documenting API that can be experimented with in the web browser. I also has equivalent curl statements for API actions done in JavaScript. Here is a link to the [API documentation](http://3.104.75.129/api/docs) it provided for this project.

- A [Dockerfile](https://docs.docker.com/build/concepts/dockerfile/) has also been developed and can be view [here.](https://github.com/apcave/acoustic-calcs/blob/main/run_docker) It defines a minimal operating system with just enough software to run the API server. Containerizing the application result in it being able to be deployed in Kubernetes or as is the can here on an AWS EC2. The Dockfile is part of the CI/CD testing and deploy process.

- Flake8 and fprettify are used for formatting and linting of the python and fortran code respectively.

## Getting Started

The project is best debugged natively on your development computer there are installation scripts to get you running on Mac OSX, Debain and Windows.

The scripts install;

- Git and clone the project into the directory where they run.
- Python, setup a virtual environment install the required packages.
- gcc and gfortan (ninja) for compiling the fortran code into a python module.
- PostgreSQL a server is installed on the local machine and the database is intialialised.
- Minor utilites and further scripts in the respository directory.

For a mac OSX run the follwing command, the script it runs can be viewed [here.](https://github.com/apcave/acoustic-calcs/blob/main/setup/setup_osx.sh)

```bash
 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/apcave/acoustic-calcs/refs/heads/main/setup/setup_osx.sh)"
```

Alternatively, for Debain run the following command, the script it runs can be viewed [here.](https://github.com/apcave/acoustic-calcs/blob/main/setup/setup_debian.sh)

```bash
 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/apcave/acoustic-calcs/refs/heads/main/setup/setup_debian.sh)"
```

Finally for Windows run the following command, the script is runs can be viewed [here.](https://github.com/apcave/acoustic-calcs/blob/main/setup/setup_win.ps1)

```bash
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/apcave/acoustic-calcs/refs/heads/main/setup/setup_win.ps1" -OutFile "setup_win.ps1"
powershell -ExecutionPolicy Bypass -File "setup_win.ps1"
```

These scripts will install the required software, create a database running locally, build and test on both a docker container and running on the host OS. After installation the API can be inspected in your browser on the local machine.

### Project Configuration

The installation scripts create a file with environmental variables in the project root called ".env". This file contains the details of the Postgres accounts used to configure and run the database. The hostname and port of the PostreSQL server to be used. Development acoustic-cals accounts used to manage, access and test the API while in development. There is also some Django configurations including a debug flag, password hashing secret

### Mac OSX and Debian

On Debain to issue python commands from the terminal requires the environment variables to be sourced run.

```bash
source ./run_native.sh
```

This will launch the python virtual enviroment, get the enviroment variables from file, start the django server in the backgound on [localhost:8080](http://localhost:8080) and do a runtime test of the server.

To run django in the docker container run,

```bash
./run_docker.sh
```

this will build the container, do software development testing, test the ports internally and run the django server on port [localhost:80](http://localhost:80).

#### Further Scripts and Commands

There is a script to delete the database and recreate the tables and users.

```bash
./scripts/recreate_user.ps
```

### Windows

After completing the installation process reboot and run the next script inside the repository clone.

```bash
C:\acoustic-calcs>.\windows\setup_reboot.ps1
```

It will configure python, build and copy the fortran code, configure the database, create users, test the db connection and run the TTD tests.
Finally it runs the Django development server on 0.0.0.0 port 80, native on your host.
Close the development server and execute the following to start a docker container on you host.

```bash
C:\acoustic-calcs>.\windows\run_docker.ps1
```

This will build the docker container with the django code inside, boot the container, run test and starts to serve on 0.0.0.0 port 80 using the production server code.
Note the native and the virtual servers both attach to 0.0.0.0 port 80 so an error will occur if you attempt to run both at the same time.

#### Windows further details

The project has a number of powershell scripts that can be used during development.

Configure the environment variables prior to running python commands in your powershell.

```bash
C:\acoustic-calcs>.\start_win.ps1
```

Run the development server in a powershell.

```bash
C:\acoustic-calcs>.\windows\run_native.ps1
```

Run the the production server in a docker container.

```bash
C:\acoustic-calcs>.\windows\run_docker.ps1
```

Run curl based API tests on any IP using your .env file settings.

```bash
C:\acoustic-calcs>.\windows\tests_url.ps1 localhost
```

### Django Commands

There are a main commands to be aware of when using the server;

```bash
python ./acoustic/manage.py runserver 0.0.0.0:8080
```

starts the server on port 8080,

```bash
python ./acoustic/manage.py test
```

starts the Test Driven Development (TTD) tests,

```bash
python ./acoustic/manage.py test_db_connection
```

tests the database connection.

## Project Overview

There is a single fortran file [here](https://github.com/apcave/acoustic-calcs/blob/main/composite-sim/src/levesque.F90) that implements a model of the response of a composite material to shear and compression pressure wave.

The scripts setup the environment and use the command f2py to compile the correctly formatted fortran into a python module. The python module need to be call in a specific way for data integrity and memory usage it is called in this file [here](https://github.com/apcave/acoustic-calcs/blob/main/acoustic/composite/utils/run_sim.py).

Django provides user authentication and management, JWT software tokens and an API endpoint that accepts a composite material model in JSON and results that model with simulation results. The users can be remotely administered using the end-point [http://localhost/api/admin](http://localhost/api/admin) and are is Swagger documentation at [http://localhost/api/docs](http://localhost/api/admin) where the API can be tested.

The operations side of this development project has been significant, getting the project to run on OSX, Debain, Windows and in a container running in Github actions, deploying to an AWS EC2. Took a lot of scripting and testing. The result is a flexible project which I can use to either extend the API that I have or create new servers for commercial application or other physics projects.
