minecraft-data
==============

This library provides support for creating mindcraft levels (.mca files) and commands (that can be entered in the game console).

At the moment it only contains low-level primatives. The support for
levels, blocks, player, etc is relatively complete, though has not yet
been updated for Minecraft 1.10.

The next step is to add some high level combinators.

An example showing how to generate a chunk in a level can be found at:

https://github.com/stepcut/minecraft-data/blob/master/utils/GenWorld.hs

At the moment, the only command which is implemented is give:

https://github.com/stepcut/minecraft-data/blob/master/Minecraft/Command.hs

Patches welcome!

- jeremy
