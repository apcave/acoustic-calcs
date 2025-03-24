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

For a mac OSX run the follwing command, the script it runs can be viewed [here.](https://github.com/apcave/acoustic-calcs/blob/main/setup/setup_osx.sh)

````bash
 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/apcave/acoustic-calcs/refs/heads/main/setup/setup_osx.sh)"
````

Alternatively, for Debain run the following command, the script it runs can be viewed [here.](https://github.com/apcave/acoustic-calcs/blob/main/setup/setup_debian.sh)

````bash
 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/apcave/acoustic-calcs/refs/heads/main/setup/setup_debian.sh)"
````

Finally for Windows run the following command, the script is runs can be viewed [here.](https://github.com/apcave/acoustic-calcs/blob/main/setup/setup_win.ps1)
````bash
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/apcave/acoustic-calcs/refs/heads/main/setup/setup_win.ps1" -OutFile "setup_win.ps1"
powershell -ExecutionPolicy Bypass -File "setup_win.ps1"
````

These scripts will install the required software, create a database running locally, build and test on both a docker container and running on the host OS. After installation the API can be inspected in your browser on the local machine.

### Mac and Debian

On Debain to issue python command from the terminal requires the environment variables to be exported using the following command.

````bash
export $(cat .env | xargs)

````

Otherwise shell scripts can be used

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

### Environment Variables

Environment variables can be set using at .env file at the project root for development or configured using server secrets.

#### Authentication Hash

```bash
AUTH_SECRET=
```

To generate a secure authentication secret, you can use the following.

```bash
node -e "console.log(require('crypto').randomBytes(32).toString('hex'))"
```

Copy the result into the environment variable or .env file.

#### Google Authentication Provider

```bash
GOOGLE_CLIENT_ID=
GOOGLE_CLIENT_SECRET=
```

Follow the instruction from NextAuth.js [here](https://next-auth.js.org/providers/google). You will need a google developer account

#### MongoDB variable

```bash
MONGODB_URI="mongodb+srv://<username>:<password>@<collection>.<url>/acoustic?retryWrites=true&w=majority"
```

The web application uses a [MongoDB](https://mongodb.com) database for persistent storage. You will need a [connection string](https://www.mongodb.com/resources/products/fundamentals/mongodb-connection-string) to a database with administrator access. You can get a database and connection with the free account.

## Running on a Development Server

Once the environmental variables are correct set simply run.

```bash
npm install
npm run dev
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

## Running on a Production Server

1. Make your own GitHub repository of this code.

2. Get an account with [Vercel](http://vercel.com) follow the instructions to setup with a GitHub account. The Vercel project will attach to a GitHub repository and the project has options for setting server secrets.

3. Push to CI/CD deploy.

## To Do

- Models page pre-rendering takes time, do an optimistic update.
- Clicking on the models in the list results in a delayed response the first time. Ensure all model pages are pre-renders when the server starts.
- When you click on run-model there no feed-back, provide a message and a spinner.
- Provide for editing of material properties after adding them to the a model. Click in the Layers section on the name to edit.
- Add edit materials button next to run simulation to add more materials to an existing model.
- More edit name and description to where it is displayed on the edit model page.
- When the shear goes from not a fluid to a fluid the UI errors.
- When frequency is swept there needs to be a way of setting the angle.
- When the angle is swept there needs to be a way to setting the frequency.
- Add more animations work on UX.
- Animate loading model also add spinner.
- Work on calculations code.
- Once data is available work on graphs and results section.
- Once data is available add feature to down-load it through the browser.
