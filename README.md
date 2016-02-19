[![Hackage version](https://img.shields.io/hackage/v/freesound.svg?style=flat)](http://hackage.haskell.org/package/freesound)
[![Build Status](https://img.shields.io/travis/kaoskorobase/freesound.svg?style=flat&branch=develop)](https://travis-ci.org/kaoskorobase/freesound)

This is a Haskell interface to the [Freesound API](https://www.freesound.org/docs/api).

Porting to API v2 is a work in progress and not quite finished yet. At the moment all basic resource types, such as sounds, users, packs, etc. are supported. Advanced features such as OAuth2 authentication, content based search and audio analysis resources are planned to be implemented before a 1.0 release. Have a look at [the tests](test/Sound/Freesound) to see what's supposed to work currently.
