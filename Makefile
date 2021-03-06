################################
## Makefile Template for Haskell
## Author: mkut
## Date  : 2012-05-17
## Rev   : 0.1.1
################################

## project settings
TARGET = main
SOURCES_BOOT =
SOURCES = \
	Main.hs \
	TableGameCombinator/Core.hs \
	TableGameCombinator/Choice.hs \
	TableGameCombinator/Phase.hs \
	TableGameCombinator/Phase/Enum.hs \
	TableGameCombinator/Phase/Enum/MultiPlayer.hs \
	TableGameCombinator/State.hs \
	TableGameCombinator/Zone.hs \
	TableGameCombinator/Tag.hs \
	TableGameCombinator/Player.hs \
	Sample/Dominion.hs \
	Sample/Dominion/Base.hs \
	Sample/Dominion/Phase.hs \
	Sample/Dominion/CardData.hs \
	Sample/Dominion/Prim.hs \
	Sample/Dominion/IOImpl/Console.hs
MILIBS = 

## command settings
HC = ghc
HCFLAGS = --make -O

## directory settings
MILIB_DIR = /home/mkut/milib/haskell

## file name macros
OBJS = $(SOURCES:%.hs=%.hi) $(SOURCES:%.hs=%.o) $(SOURCES_BOOT:%.hs-boot=%.hi-boot) $(SOURCES_BOOT:%.hs-boot=%.o-boot)
MILIB_SRC = $(MILIBS:%=$(MILIB_DIR)/%.hs)

## make rules
.PHONY: default clean

default: $(TARGET)

$(TARGET): $(SOURCES) $(SOURCES_BOOT) $(MILIB_SRC)
	$(HC) $(HCFLAGS) -o $@ $(SOURCES) $(MILIB_SRC)

clean:
	rm -f $(TARGET) $(OBJS)
