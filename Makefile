################################
## Makefile Template for Haskell
## Author: mkut
## Date  : 2012-05-17
## Rev   : 0.1.1
################################

## project settings
TARGET = main
SOURCES = \
	Main.hs \
	TableGameCombinator/Core.hs \
	TableGameCombinator/Phase.hs \
	Sample/Dominion.hs \
	Sample/Dominion/Base.hs \
	Sample/Dominion/Card.hs \
	Sample/Dominion/Phase.hs \
	Sample/Dominion/CardData.hs \
	Sample/Dominion/IO.hs \
	Sample/Dominion/PrimEffect.hs
MILIBS = 

## command settings
HC = ghc
HCFLAGS = --make -O

## directory settings
MILIB_DIR = /home/mkut/milib/haskell

## file name macros
OBJS = $(SOURCES:%.hs=%.hi) $(SOURCES:%.hs=%.o)
MILIB_SRC = $(MILIBS:%=$(MILIB_DIR)/%.hs)

## make rules
.PHONY: default clean

default: $(TARGET)

$(TARGET): $(SOURCES) $(MILIB_SRC)
	$(HC) $(HCFLAGS) -o $@ $^
	rm $(OBJS)

clean:
	rm -f $(TARGET) $(OBJS)
