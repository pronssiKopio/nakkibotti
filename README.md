# Telegram Bots

This projects serves as a means of testing and developing Telegram bots for Programming Studio 1 2021 exercise

Your bot will *inherit* a Basic Bot, which gives you a list of useful commands:

https://version.aalto.fi/gitlab/oseppala/botti-2021/-/blob/master/src/main/scala/s1/telegrambots/BasicBot.scala

**You don't need to understand how the methods are implemented.**

For the absolute simplest example of how that bot base can be used, check out the Reverse-bot.
If defines a simple method that takes a String parameter and returns that String reversed.
The bot is then told to run that method any time any text is written to the chat.

see the example here:

https://version.aalto.fi/gitlab/oseppala/botti-2021/-/blob/master/src/main/scala/s1/telegrambots/examples/ReverseBot.scala



## Bot token

You need to create a file called **bot_token.txt** which contains your bot_token and place it in the root directory for the code to work. The token is given to you by the BotFather -bot in Telegram.

The **bot_token.txt** -file should never be pushed into the repository for safety reasons. Our .gitignore -file should prevent this from happening.

## Bot4s

The exercise base uses the **Bot4s** -library by Alfonso Peterssen.
You don't need to use the library directly if you use the course-provided helper functions, but in case you want to go further, check out the examples and documentation of that project.

## Remember to ask your teaching assistant and make use of the exercise groups!

But even more importantly - Discuss with your Teaching assistant if you think of a cool feature, but dont't know how to implement it. This course is all about collaboration and learning from others - We can figure out solutions together.
