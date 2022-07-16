#!/usr/bin/env python3

import discord
from discord.ext import commands
import os
from dotenv import load_dotenv
import datetime

# my imports
from database import Database
from cog      import ShepherdCog, Shephelp


def main():

    print('Shepherd 2.0')

    # load Discord token from file and stuff
    load_dotenv()
    token = os.getenv('DISCORD_TOKEN')
    database_file = os.getenv('DATA')

    # start database in the data file
    db = Database(database_file)

    # turn on intents to acess channel.members (important)
    intents = discord.Intents.default()
    intents.members = True

    # start the client
    client = commands.Bot(command_prefix='!', intents=intents)
    client.add_cog(ShepherdCog(client, db))
    client.help_command = Shephelp()
    client.run(token)


if __name__ == "__main__":
    main()


