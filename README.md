
# guile-redis

guile-redis is a Guile module for the [Redis](http://redis.io) key-value data
store. It provides all commands up to Redis 6.0 and supports multiple
commands, pipelining and Pub/Sub.

# Installation

Download the latest tarball and untar it:

- [guile-redis-2.0.0.tar.gz](http://download.savannah.gnu.org/releases/guile-redis/guile-redis-2.0.0.tar.gz)

If you are cloning the repository make sure you run this first:

    $ autoreconf -vif

Then, run the typical sequence:

    $ ./configure --prefix=<guile-prefix>
    $ make
    $ sudo make install

Where <guile-prefix> should preferably be the same as your system
Guile installation directory (e.g. /usr).

If everything installed successfully you should be up and running:

    $ guile
    scheme@(guile-user)> (use-modules (redis))
    scheme@(guile-user)>

It might be that you installed guile-redis somewhere differently than
your system's Guile. If so, you need to indicate Guile where to find
guile-redis, for example:

    $ GUILE_LOAD_PATH=/usr/local/share/guile/site guile

# Usage

## Procedures

The main interface to the Redis server is really simply. It consists
on three procedures, the most important one being /redis-send/ which
basically sends commands to the server.

- (**redis-connect** #:key host port) : Establish a connection to the redis
  server at the given *host* and *port* and return a Redis connection.

  - *host* : it defaults to 127.0.0.1.
  - *port* : it defaults to 6379.

- (**redis-close** connection) : Close the *connection* to the Redis server.

- (**redis-send** connection commands) : Send one or more *commands* to the
  Redis *connection*. When sending multiple commands a list of all the replies
  is returned.

  **Returns** : A reply or a list of replies if multiple commands were sent.

  **Throws**

  - *redis-error* : if the Redis server returns an error. The exception has
    the error string as an argument.

  - *redis-invalid* : if the user arguments or server's answer cannot be
    parsed (hopefully, this is unlikely to happen).


## Redis commands

All commands in guile-redis are defined with lower-case and hyphenated in the
case of commands that have two or more words. For example, the command
"*CLIENT LIST*" is defined as *client-list*.

The commands take exaclty the same arguments as defined in the Redis manual
and all the arguments (if any) need to be passed as a single list. For
example:

    (bitfield '(mykey INCRBY i5 100 1 GET u4 0))

Note that, internally, guile-redis will automatically convert symbols and
numbers to strings before sending the command to Redis.


## Redis Pub/Sub

guile-redis >= 2.0.0 adds proper support for
[[https://redis.io/topics/pubsub][Redis Pub/Sub]]. The Pub/Sub commands don't
follow the approach of the rest of commands (except the *PUBSUB* command),
instead there's a procedure for each of them:

- (**redis-publish** connection channel message) : Publish the given *message*
  to *channel* using the provided Redis client *connection*. All subscribers
  to that channel will receive the message.

  **Returns** : number of clients that received the message.

- (**redis-subscribe** connection channels) : Subscribe to the given list of
  *channels* using the Redis client *connection*.

  **Returns** : a list of pairs where the first value is the subscribed
  channel and the second value is the total number of subscribed channels
  (when that channel was subscribed).

- (**redis-unsubscribe** connection [channels]) : Unsubscribe from the given
  optional list of *channels* using the Redis client *connection*. If
  *channels* is not specified it will unsubscribe from all the subscribed
  channels.

  **Returns** : a list of pairs where the first value is the unsubscribed
  channel and the second value is the total number of remaining subscribed
  channels (when that channel was unsubscribed).

- (**redis-psubscribe** connection patterns) : Subscribe to the given list of
  channel *patterns* using the Redis client *connection*.

  **Returns** : a list of pairs where the first value is the subscribed
  pattern and the second value is the total number of subscribed channel
  patterns (when that pattern was subscribed).

- (**redis-punsubscribe** connection [patterns]) : Unsubscribe from the given
  optional list of channel *patterns* using the Redis client *connection*. If
  *patterns* is not specified it will unsubscribe from all the subscribed
  patterns.

  **Returns** : a list of pairs where the first value is the unsubscribed
  pattern and the second value is the total number of remaining subscribed
  channel patterns (when that patterns was unsubscribed).

- (**redis-subscribe-read** connection) : Read the next message from one of
  the subscribed channels on the given *connection*. This is a blocking
  operation until a message is received.

  **Returns**: a couple of values, the channel where the message was received
  and the actual message.

# Examples

- Load the module:

    > (use-modules (redis))

- Create a connection:

    > (define conn (redis-connect))

- Send a single *PING* command:

    > (redis-send conn (ping))
    "PONG"

- Send a couple of *PING* commands:

    > (redis-send conn ((ping) (ping '("hello from guile-redis"))))
    ("PONG" "hello from guile-redis")

- Set a couple of keys:

    > (redis-send conn (mset '(hello "world" foo "bar")))
    "OK"

- Retrieve the keys just set above:

    > (redis-send conn (mget '(hello foo)))
    ("world" "bar")

- Subscribe to the *news* channel:

    > (redis-subscribe conn '("news"))
    (("news" . 1))

- Publish message to the *news* channel:

    > (redis-publish conn "news" "hello from guile-redis")
    1

- Unsubscribe from all channels:

    > (redis-unsubscribe conn)
    (("news" . 0))

- Finally, close the connection:

    > (redis-close conn)

# License

Copyright (C) 2013-2020 Aleix Conchillo Flaque <aconchillo@gmail.com>

This file is part of guile-redis.

guile-redis is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

guile-redis is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with guile-redis. If not, see https://www.gnu.org/licenses/.
