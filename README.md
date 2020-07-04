slack-composer
-------------------
slack-composer is a Slack bot for creating and sharing AI music using the Amper API.

Built to deploy on a Heroku instance using the following buildpack:

https://github.com/thealexgraham/heroku-buildpack-ghc

To deploy, set an environment variable called `API_KEY` to your Amper API key and push the app
to Heroku. If you would like to store the compositional information for later use, you can add
the optional `DATABASE_URL` as a third argument (see Procfile).

To run in another environment, run the `amperbot` executable with the following arguments:

`slack-composer --port {PORT} --api-key {API_KEY} --database {DATABASE_URI}`

To set up the Slack bot, create an app with a slash command that points to https://your-url.com/compose.

Note: This is a personal project and is not affliated officially with Amper Music. All code in this project
was written independantly based off of publicly available information from http://docs.ampermusic.com/.
