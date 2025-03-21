# Composite Acoustic Simulator.

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

The project is best debugged natively on your development computer, instruction are provided here for Linux / WSL and Mac.

For a mac OSX,

```bash
 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/apcave/acoustic-calcs/refs/heads/main/setup/setup_osx)"
```

alternatively for Linux or Debian,

```bash
sudo apt install -y nginx python3 gcc gfortran curl jq lsof
```

Clone the repository and prepare the python environment,

```bash
git clone https://github.com/apcave/acoustic-calcs.git
cd acoustic-calcs
python3 -m venv venv
source venv/bin/activate
```

Prepare the environment variables,

```bash
DB_NAME=acoustic_calcs
DB_USER=acoustic
DB_PASS=
DB_HOST=
DB_PORT=
DJANGO_SECRET_KEY=
DJANGO_DEBUG=1
DJANGO_ALLOWED_HOSTS=*
CALC_EMAIL=
CALC_PASS=
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
