SRC=src

all: adventure

adventure: ${SRC}/adventure.hs ${SRC}/Locations.hs ${SRC}/Engine.hs ${SRC}/Items.hs ${SRC}/StandardOptions.hs
	ghc -outputdir bin -o adventure $^