# Launching on Heroku

In order to use certain functionality from this series, like responding to text messages to our Twilio number, you'll have to have your server deployed on the web. There are a number of different services you can use for this. But my preferred free option is [Heroku](https://www.heroku.com). So to start out, you should make an account if you have not already. You should also prefer to **fork** this repository rather than **clone** this repository.

## Logging In

Once you have an account, you need to log in to it at the terminal, from the project root. You should use the `heroku login` command.

```bash
>> cd ~/APIs # Or wherever the root directory is
>> heroku login
```

This will give you a link to follow to a web browser to enter your credentials. But you can also use `-i` to enter your information on the terminal.

```bash
>> heroku login -i
heroku: Enter your login credentials
Email: ...
Password: ...
Logged in as ...
```

## Creating and Configuring the App

Now from the root of your project, create a new Heroku "app". You'll need to use the Haskell Stack "buildpack" available [here](https://github.com/mfine/heroku-buildpack-stack) with the `-b` argument. You can replace `mmh-apis` with whatever app name you would like to give your project.

```bash
>> heroku create mmh-apis \
  -b https://github.com/mfine/heroku-buildpack-stack
```

Note that whatever name you choose, you'll have to update some of the code to make one of the email links work. You'll want to update the two uses of `mmh-apis` in the [Email module](https://github.com/MondayMorningHaskell/APIs/blob/master/src/Email.hs#L64-L66) so that they instead use your app name.

Next you should "scale" the application so it has a machine to run:

```bash
>> heroku ps:scale web=1
```

If you go to your Heroku dashboard, you should see your application now. You'll need to customize it by adding various credentials related to your Twilio, Mailgun, and Mailchimp accounts. To do this, click on the application and go to the "Settings" page and find the "Config Vars" section, as seen here:

[[https://github.com/MondayMorningHaskell/APIs/blob/master/HerokuConfig.png|alt=Heroku Config Vars]]

Click "Reveal Config Vars" and you'll be able to start adding them manually. You should do this for values like `TWILIO_ACCOUNT_SID` that are used as environment variables throughout our applications.

This is also possible from the command line:

```bash
>> heroku config:set TWILIO_ACCOUNT_SID=...
>> heroku config:set TWILIO_AUTH_TOKEN=...
```

## Pushing Your Code

Finally, we need to push our application to the Heroku container. Verify that Heroku created the remote `heroku` repository for you by running the `git remote` command. You should see two lines for `heroku`, in addition to the `origin`:

```bash
>> git remote -v
heroku https://git.heroku.com/mmh-apis.git (fetch)
heroku https://git.heroku.com/mmh-apis.git (push)
origin ...
origin ...
```

If these aren't present you can also add them like so:

```bash
>> heroku git:remote -a mmh-apis
```

Then you can push your code to the container like so:

```bash
>> git push heroku master
```

You'll see terminal output indicating that Heroku recognizes your application, followed eventually by the `stack build` process. This is all happening on the remote machine, so it doesn't matter if you close your terminal or terminate the command. Once it's done, your app will be deployed! It can be accessed through the URL `https://your-app-name.herokuapp.com`. So if you go to `https://your-app-name.herokuapp.com/api/ping`, your browswer should display "Pong". The first request will always take a few extra seconds each time.

## Updating the Code

Once your code is deployed, you can make any code updates you want. To push your changes to the cloud, you have to commit them to Github, and push to the `heroku` remote again.

```bash
>> git add .
>> git commit -m "Make updates"
>> git push heroku master
```

By default, this will run the complete server in the [FullServer module](https://github.com/MondayMorningHaskell/APIs/blob/master/src/FullServer.hs). This is because the [Procfile](https://github.com/MondayMorningHaskell/APIs/blob/master/Procfile) indicates `run-full-server` as the `web` option:

```
web: run-full-server
```

If instead, you want to run the more basic server from the first part of the series, you can change the Procfile to use `run-sms-server` instead.

```
web: run-full-server
```

Then commit your changes and push, as before.
