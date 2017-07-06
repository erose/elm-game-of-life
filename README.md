###Development

To set up a dev environment:

  * `npm install -g elm`
  * `elm-reactor`
  * navigate to `http://localhost:8000`. Note that the first time you do this, `elm-reactor` will install this project's dependencies (from `elm-package.json`), which takes a while.

After making a change, reload the page to see the results (or the compiler errors).
