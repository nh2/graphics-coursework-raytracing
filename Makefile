.PHONY: config \
        test

config:
	cabal configure --disable-library-profiling --disable-shared

test:
	runhaskell test/Tests.hs


.PHONY: scene1 scene2 scene3 scene4 scene5 allScenes

scene1:
	dist/build/raycast/raycast --input scenes/scene1.txt --size 200 200 --output scene1.ppm --depth 9 10 depth1.ppm
scene2:
	dist/build/raycast/raycast --input scenes/scene2.txt --size 200 200 --output scene2.ppm --depth 8 12 depth2.ppm
scene3:
	dist/build/raycast/raycast --input scenes/scene3.txt --size 200 200 --output scene3.ppm --depth 8 12 depth3.ppm
scene4:
	dist/build/raycast/raycast --input scenes/scene4.txt --size 200 200 --output scene4.ppm --depth 13 16 depth4.ppm
scene5:
	dist/build/raycast/raycast --input scenes/scene5.txt --size 300 300 --output scene5.ppm --depth 1 7 depth5.ppm
scene6:
	dist/build/raycast/raycast --input scenes/scene6.txt --size 200 200 --output scene6.ppm --depth 9 13 depth6.ppm
allScenes: scene1 scene2 scene3 scene4 scene5 scene6
