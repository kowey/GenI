---
title: Configuration file
---

In addition to the command line arguments, GenI reads an optional configuration
file.

Right now, the configuration file is only used to control the GenI logger.

## Logger

The logger outputs detailed information about what the various components
that make up GenI think that they're doing at any particular time.

At the time of this writing, the logger isn't much used, but the morphology
module is covered somewhat

- Loggers are organised in an tree hierarchy using dotted notation
  (so for example, the logger "NLP.GenI" is the parent of "NLP.GenI.Morphology",
  which is the sister of "NLP.GenI.LexicalSelection"

- Loggers have priorities ranging from DEBUG to EMERGENCY. You can
  specify for each logger a cut-off priority.  If I set the priority
  for logger "Foo" to WARNING, only logs from WARNING and up will be
  shown for loggers in the "Foo" hierarchy.

- Sub-loggers can only hide more information (by raising the priority).
  The  baseline is the priority of its ancestors

- The root logger has a priority of DEBUG by default

- If you don't specify any loggers, we give "NLP.GenI" a priority
  of INFO


- all fields optional
* name: module name (topmost is NLP.GenI)
* handler:
  - file "FILENAME"
  - stderr
* level: (see hslogger for levels)
* format: (see [System.Log.Formatter](http://hackage.haskell.org/packages/archive/hslogger/latest/doc/html/System-Log-Formatter.html#t:LogFormatter)
doc)

## Example

* Windows XP:
* Windows 7: C:\Users\xxx\AppData\Roaming\geni\config.yaml
* Linux/MacOS X: $HOME/.geni/config.yaml

    logging:
      -
        name     : NLP.GenI
        handler  : file C:/geni.log
      -
        name     : NLP.GenI.LexicalSelection
        handler  : stderr
      -
        name     : NLP.GenI.Morphology
        level    : DEBUG
