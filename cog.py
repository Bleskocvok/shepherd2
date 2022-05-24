

from discord.ext import commands
import discord

from typing import Optional, List, Any
import asyncio

# my imports
from database import Database
from table import create_table
import utils


EMBED_COLOR = 0x99F7A7


def embed(text: str, title: Optional[str] = None) -> discord.Embed:
    res = discord.Embed(color=EMBED_COLOR)
    res.add_field(name=title, value=text, inline=False)
    return res


def embed_table(rows: Any,
                header: List[str],
                title: Optional[str] = None,
                info:  Optional[str] = None) -> discord.Embed:

    data = rows.fetchall()
    tab = create_table(header, data, line = ('`', '`'),
                                     head = ('**`', '`**'))
    res = discord.Embed(color=EMBED_COLOR)
    res.add_field(name=title, value=tab, inline=False)
    if info is not None:
        res.set_footer(text=info)
    return res


class ShepherdCog(commands.Cog):

    def __init__(self, bot: commands.Bot, database: Database):
        self.bot = bot
        self.db = database


    @commands.Cog.listener()
    async def on_command_error(self, ctx, error):
        print('ERROR:', error, flush=True)


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
    async def list(self,
                   ctx,
                   interval: str = 'day',
                   user: discord.User = None):

        user = ctx.author if user is None else user
        days = utils.str_to_days(interval)
        rows = self.db.get_interval(days, user.id, ctx.guild.id)
        e = embed_table(rows,
                        ['exercise', 'amount', 'unit', 'timestamp'],
                        f'Recorded activity – {interval}',
                        f'Data for {user.name}')
        await ctx.send(embed=e)


    @commands.command(help='List last N recorded exercises')
    async def last(self,
                   ctx,
                   n: int = 10,
                   user: discord.User = None):

        user = ctx.author if user is None else user
        rows = self.db.get_last(n, user.id, ctx.guild.id)
        e = embed_table(rows,
                        ['exercise', 'amount', 'unit', 'timestamp'],
                        f'Recorded activity – last {n}',
                        f'Data for {user.name}')
        await ctx.send(embed=e)


    @commands.command(help='List stats for given time period')
    async def stats(self,
                    ctx,
                    interval: str = 'day',
                    user: discord.User = None):

        user = ctx.author if user is None else user
        days = utils.str_to_days(interval)
        rows = self.db.get_total_by_days(days, user.id, ctx.guild.id)
        e = embed_table(rows,
                        ['day', 'exercise', 'total', 'unit', 'tmp'],
                        f'Total stats – {interval}',
                        f'Data for {user.name}')
        await ctx.send(embed=e)


    @commands.command(help='List total amount of recorded exercises for given time period')
    async def total(self,
                    ctx,
                    interval: str = 'day',
                    user: discord.User = None):

        user = ctx.author if user is None else user
        days = utils.str_to_days(interval)
        rows = self.db.get_total(days, user.id, ctx.guild.id)
        e = embed_table(rows,
                        ['exercise', 'total', 'unit', 'day'],
                        f'Total – last {interval} days',
                        f'Data for {user.name}')
        await ctx.send(embed=e)


    @commands.command(help='List exercise types')
    async def exercises(self, ctx):

        rows = self.db.get_exercise_types()
        e = embed_table(rows, ['type', 'unit'], 'Exercises')
        await ctx.send(embed=e)


    @commands.command(help='Stores the amount of exercise you did')
    async def did(self, ctx, amount : int, type: str):

        self.db.add_record(type, amount, ctx.author.id, ctx.guild.id)
        await ctx.message.add_reaction('\N{FLEXED BICEPS}')


