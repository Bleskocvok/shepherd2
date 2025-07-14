#!/usr/bin/env python3


import sqlite3


TABLE_SCHEMAS = """
    CREATE TABLE IF NOT EXISTS exercise_type
    (
        id              INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        name            varchar(200),
        unit            varchar(10),

        UNIQUE(name, unit)
    );

    CREATE TABLE IF NOT EXISTS record
    (
        id              INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        user            INTEGER NOT NULL,
        server          INTEGER NOT NULL,
        exercise_id     INTEGER NOT NULL,
        value           decimal(5, 2),
        time            timestamp,

        CONSTRAINT one
            FOREIGN KEY (exercise_id)
            REFERENCES exercise_type(id)
    );

    CREATE TEMP VIEW IF NOT EXISTS
        record_exercise(ex_id, user, server, ex_name, unit, value, time)
    AS
        SELECT
            exercise_type.id,
            user,
            server,
            exercise_type.name,
            exercise_type.unit,
            value,
            time
        FROM
            record INNER JOIN exercise_type
                    ON record.exercise_id = exercise_type.id
        ORDER BY
            time DESC;
"""


DEFAULT_TYPES = """
    INSERT OR IGNORE INTO
        exercise_type(name, unit)
    VALUES
        ('pushup', 'times')
        , ('pullup', 'times')
        , ('plank',  'sec')
        , ('run',    'km')
        , ('walk',   'km')
        , ('crunch', 'times')
        , ('badminton', 'min')
        ;
"""


class Database:


    def __init__(self, filename: str):
        self.con = sqlite3.connect(filename)
        self.cur = self.con.cursor()

        self.cur.executescript(TABLE_SCHEMAS)
        self.cur.executescript(DEFAULT_TYPES)


    def close(self):
        self.con.close()


    def add_record(self,
            type: str,
            value: int,
            user: int,
            server: int) -> None:

        mapping = {
            'usr' : user,
            'ser' : server,
            'typ' : type,
            'val' : value
        }

        self.con.execute('''
            INSERT INTO
                record(user, server, exercise_id, value, time)
            VALUES
                (:usr,
                 :ser,
                 (SELECT id
                    FROM exercise_type
                    WHERE name = :typ
                    LIMIT 1),
                 :val,
                 datetime('now', 'localtime'))
        ''', mapping)

        self.con.commit()


    def get_last(self,
            count: int,
            user: int,
            server: int) -> sqlite3.Cursor:

        mapping = {
            'usr' : user,
            'srv' : server,
            'cnt' : count
        }

        r = self.con.execute('''
            SELECT
                ex_name,
                value,
                unit,
                time(time) || ' ' || date(time)
            FROM
                record_exercise
            WHERE
                user = :usr
                AND server = :srv
            LIMIT
                :cnt
        ''', mapping)

        return r


    def get_interval(self,
            days: int,
            user: int,
            server: int) -> sqlite3.Cursor:

        mapping = {
            'usr' : user,
            'srv' : server
        }

        r = self.con.execute(f'''
            SELECT
                ex_name,
                value,
                unit,
                time(time) || ' ' || date(time)
            FROM
                record_exercise
            WHERE
                user = :usr
                AND server = :srv
                AND date(time) > date('now', '-{days} day') -- no sql injection here
                                                            -- ‹days› is ‹int›
            LIMIT 50
        ''', mapping)

        return r


    def get_total(self,
            days: int,
            user: int,
            server: int) -> sqlite3.Cursor:

        mapping = {
            'usr' : user,
            'srv' : server
        }

        r = self.con.execute(f'''
            SELECT
                ex_name,
                SUM(value),
                unit
            FROM
                record_exercise
            WHERE
                user = :usr
                AND server = :srv
                AND date(time) > date('now', '-{days} day') -- no sql injection here
                                                            -- ‹days› is ‹int›
            GROUP BY
                ex_id
            LIMIT 50
        ''', mapping)

        return r


    def get_exercise_by_days(self,
            exercise: str,
            days: int,
            user: int,
            server: int) -> sqlite3.Cursor:

        mapping = {
            'exe' : exercise,
            'usr' : user,
            'srv' : server
        }

        r = self.con.execute(f'''
            WITH RECURSIVE
                numbers(n) AS
                (
                    SELECT 0
                    UNION ALL
                    SELECT n + 1
                        FROM numbers
                    LIMIT {days}
                ),
                days(day) AS
                (
                    SELECT date('now', '-' || n || ' day')
                    FROM numbers
                ),
                records(ex_id, user, server, ex_name, unit, value, time) AS
                (
                    SELECT *
                    FROM record_exercise
                    WHERE user = :usr
                        AND server = :srv
                        AND ex_name = :exe
                )
            SELECT
                CASE strftime('%w', day)
                    WHEN '0' THEN 'Sun'
                    WHEN '1' THEN 'Mon'
                    WHEN '2' THEN 'Tue'
                    WHEN '3' THEN 'Wed'
                    WHEN '4' THEN 'Thu'
                    WHEN '5' THEN 'Fri'
                    WHEN '6' THEN 'Sat'
                    ELSE 'NaN'
                END,
                IFNULL(SUM(value), 0),
                (SELECT unit FROM records LIMIT 1),
                strftime('%d/%m', day)
            FROM
                days LEFT JOIN records
                            ON date(records.time) = day
            GROUP BY
                day
            ORDER BY
                day DESC
        ''', mapping)

        return r


    def get_date_ago(self, days: int):

        return self.con.execute(f'''
            SELECT date('now', '-' || {days} || ' day')
        ''')


    def get_total_by_days(self,
            days: int,
            user: int,
            server: int) -> sqlite3.Cursor:

        mapping = {
            'usr' : user,
            'srv' : server
        }

        r = self.con.execute(f'''
            SELECT
                CASE strftime('%w', time)
                    WHEN '0' THEN 'Sun'
                    WHEN '1' THEN 'Mon'
                    WHEN '2' THEN 'Tue'
                    WHEN '3' THEN 'Wed'
                    WHEN '4' THEN 'Thu'
                    WHEN '5' THEN 'Fri'
                    WHEN '6' THEN 'Sat'
                    ELSE 'NaN'
                END,
                ex_name,
                SUM(value),
                unit
            FROM
                record_exercise
            WHERE
                user = :usr
                AND server = :srv
                AND date(time) > date('now', '-{days} day') -- no sql injection here
                                                            -- ‹days› is ‹int›
            GROUP BY
                date(time),
                ex_id
            ORDER BY
                date(time) DESC
            LIMIT 50
        ''', mapping)

        return r


    def get_exercise_types(self) -> sqlite3.Cursor:
        r = self.con.execute('''
            SELECT
                name,
                unit
            FROM
                exercise_type
            ORDER BY
                name DESC
        ''')

        return r


# def main():

#     db = Database('data.db')

#     db.add_record('pushup', 1, 123, 456)
#     # import time
#     # time.sleep(3)
#     db.add_record('walk', 2, 123, 456)

#     rs = db.get_last(10, 123, 456)
#     for row in rs:
#         print(row)

#     db.close()

#     import table
#     r = table.create_table(['alfa', 'beta', 'gamma'],
#                            [('aaaaaaaaaa', 123456, 1),
#                             ('b', 123, 0)])
#     print(r)


# if __name__ == '__main__':
#     pass
#     main()

