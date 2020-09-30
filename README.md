# Telegram Bots

This projects serves as a means of testing and developing Telegram bots for OS1 2020 exercise

Your bot will *inherit* a Basic Bot, which gives you a list of useful commands:

https://version.aalto.fi/gitlab/oseppala/botti_2020/-/blob/master/src/main/scala/s1/telegrambots/BasicBot.scala

**You don't need to understand how the methods are implemented.**

For the absolute simplest example of how that bot base can be used, check out the Reverse-bot.
If defines a simple method that takes a String parameter and returns that String reversed.
The bot is then told to run that method any time any text is written to the chat.

see the example here:

https://version.aalto.fi/gitlab/oseppala/botti_2020/-/blob/master/src/main/scala/s1/telegrambots/examples/ReverseBot.scala



## Bot token

You need to create a file called **bot_token.txt** which contains your bot_token and place it in the root directory for the code to work. The token is given to you by the BotFather -bot in Telegram.

The **bot_token.txt** -file should never be pushed into the repository for safety reasons. Our .gitignore -file should prevent this from happening.

## TelegramBot4s

The exercise base uses the **TelegramBot4s** -library by Alfonso Peterssen. *We use the version 3 of the library for it's simpler structure.*
You don't need to use the library directly if you use the course-provided helper functions, but in case you want to go further, version 3 can be found here:

https://github.com/bot4s/telegram/tree/v3
