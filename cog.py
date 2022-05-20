

from discord.ext import commands
import discord

from typing import Optional, List
import asyncio

# my imports
from database import Database
from table import create_table
import utils


class ShepherdCog(commands.Cog):

    def __init__(self, bot: commands.Bot, database: Database):
        self.bot = bot
        self.db = database


    @commands.Cog.listener()
    async def on_command_error(self, ctx, error):
        print('ERROR:', error)


    @commands.Cog.listener()
    async def on_ready(self):
        print(f'Connected as {self.bot.user.name}', flush=True)


    # @commands.command(help='Schedules an alarm')
    # async def alarm(self, ctx, time):
    #     pass


    # @commands.command(help='Cancels scheduled alarm')
    # async def cancel(self, ctx):
    #     pass


    # @commands.command(help='Status')
    # async def status(self, ctx):
    #     pass


    @commands.command(help='List recorded exercises for given time period')
    async def list(self, ctx, interval: str = 'day'):
        days = utils.str_to_days(interval)
        rows = self.db.get_interval(days, ctx.author.id, ctx.guild.id)  \
                      .fetchall()
        table = create_table(['exercise', 'amount', 'unit', 'timestamp'], rows)
        await ctx.send(f'```{table}```')


    @commands.command(help='List last N recorded exercises')
    async def last(self, ctx, n: int = 10):
        rows = self.db.get_last(n, ctx.author.id, ctx.guild.id)  \
                      .fetchall()
        table = create_table(['exercise', 'amount', 'unit', 'timestamp'], rows)
        await ctx.send(f'```{table}```')


    @commands.command(help='List exercise types')
    async def exercises(self, ctx):

        rows = self.db.get_exercise_types().fetchall()
        table = create_table(['type', 'unit'], rows)
        await ctx.send(f'```{table}```')


    @commands.command(help='Stores the amount of exercise you did')
    async def did(self, ctx, amount : int, type: str):

        self.db.add_record(type, amount, ctx.author.id, ctx.guild.id)
        await ctx.message.add_reaction('\N{FLEXED BICEPS}')


