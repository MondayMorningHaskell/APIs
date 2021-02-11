# APIs

This is the companion Github repository for the Monday Morning Haskell [APIs Series](https://www.mmhaskell.com/apis). You can find all the code from that series here. This series shows you how to use several different APIs and platforms in Haskell, so to make full use of the code here, you'll have to make some accounts for various services.

## Required Services

## Environment Variables

An important lesson of using external services with API keys is that you should use environment variables to store these keys so that they are not visible in your code. Here's a list of all the environment variables you'll need to set, either on your local machine, or on a remote machine, for this code to work.

1. `TWILIO_ACCOUNT_SID`
2. `TWILIO_AUTH_TOKEN`
3. `TWILIO_PHONE_NUMBER` - A number that you must buy (for $1) from Twilio to send and receive text messages. Include country code and area code (e.g. `+15559814512`).
4. `TWILIO_USER_NUMBER` - Your own phone number, which you must verify on your Twilio account so it can send and receive messages to the Twilio number.
5. `MAILGUN_DOMAIN` - can be the sandbox domain
6. `MAILGUN_API_KEY`
7. `MAILGUN_REPLY_ADDRESS`
8. `MAILGUN_USER_ADDRESS` - Your own email that you've verified with Mailgun so that it can receive messages from the sandbox domain.
9. `MAILCHIMP_API_KEY`
10. `MAILCHIMP_LIST_NAME`
11. `MAILCHIMP_BASE_URL` - Should be something like `https://us7.api.mailchimp.com/3.0`, likely with a different "region" code instead of `us7`.
12. `PORT` - A port to run the server from. Note that Heroku will set this for you automatically.

## Running the Code Locally

You can build the code and run (most of) it locally. To do this, use:

```bash
>> stack build
```

Assuming you've set up the accounts and environment variables, you can run some basic tasks using hard-coded functions in GHCI:

```bash
>> stack ghci
>> SMS.sendBasicMessage
>> Email.sendBasicEmail
```

Using `sendBasicMessage` should send a text message from your Twilio number to your personal number. Running `sendBasicEmail` should send an email to your personal email using mailgun.

You can also run the "full" server locally using the `run-full-serever` executable:

```haskell
>> stack exec run-full-server
```

When running your server locally, **you cannot use the Twilio callbacks**. So a local server cannot process text messages that you send from your personal phone to the Twilio number. However, you can still construct an HTTP message in a service like Postman to mimic the message your server will get to test functionality. You can also make use of the Mailchimp "subscribe" handler.

You can also run a more limited server (from [part 1](https://www.mmhaskell.com/apis/twilio) of the series) by using `run-sms-server`:

```haskell
>> stack exec run-sms-server
```

## Running on Heroku

To get the full functionality demonstrated in this series, you'll need to deploy your code on the web. The easiest way to do this is using [Heroku](https://heroku.com). Take a look at [this document](https://github.com/MondayMorningHaskell/APIs/blob/master/Heroku.md) for a detailed breakdown of how to get this code working on Heroku.

## File Guide:

### [Part 1: Twilio](https://mmhaskell.com/apis/twilio)

* [SMS Module](https://github.com/MondayMorningHaskell/APIs/blob/master/src/SMS.hs)
* [SMS Server](https://github.com/MondayMorningHaskell/APIs/blob/master/src/SMSServer.hs)

### [Part 2: Mailgun](https://mmhaskell.com/apis/mailgun)

* [Email Module](https://github.com/MondayMorningHaskell/APIs/blob/master/src/Email.hs)
* [Full Server](https://github.com/MondayMorningHaskell/APIs/blob/master/src/FullServer.hs)

### [Part 3: Mailchimp](https://mmhaskell.com/apis/mailchimp)

* [Subscribers Module](https://github.com/MondayMorningHaskell/APIs/blob/master/src/Subscribers.hs)
* [Full Server](https://github.com/MondayMorningHaskell/APIs/blob/master/src/FullServer.hs)
