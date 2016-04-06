# GBot [WIP]

Github automation bot.

Development
===========

```
export GBOT_GITHUB_CLIENT_ID=...
export GBOT_GITHUB_CLIENT_SECRET=...

docker-compose run dev stack install yesod-bin
docker-compose run dev stack build
docker-compose run --service-ports dev stack exec yesod devel
```
