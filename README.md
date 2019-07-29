# minecraft-data [![Hackage Status](https://img.shields.io/hackage/v/minecraft-data.svg)][hackage] [![Build Status](https://travis-ci.org/stepcut/minecraft-data.svg?branch=master)](https://travis-ci.org/stepcut/minecraft-data)

[hackage]: https://hackage.haskell.org/package/minecraft-data

This library provides support for creating mindcraft levels (.mca files) and commands (that can be entered in the game console).

At the moment it only contains low-level primatives. The support for
levels, blocks, player, etc is relatively complete, though has not yet
been updated for Minecraft 1.10.

The next step is to add some high level combinators.

An example showing how to generate a chunk in a level can be found at:

https://github.com/stepcut/minecraft-data/blob/master/utils/GenWorld.hs

At the moment, the only command which is implemented is `Give`:

https://github.com/stepcut/minecraft-data/blob/master/Minecraft/Command.hs

Patches welcome!


Much of the data comes from here:

http://minecraft.gamepedia.com/Data_values

This site looks promising as well:

https://www.digminecraft.com/lists/item_id_list_pc.php

The flattening in 1.13 and later,

http://llbit.se/?p=3274

The building combinators were introduced by Tim Philip Williams is this blog post

http://www.timphilipwilliams.com/posts/2019-07-25-minecraft.html

- jeremy


