# mlkit-charting
Charting of MLKit performance of benchmarks over time

# Building the site

The site is built by typing

```
$ make deploy
```

You can now view the generated site as follows:

```
$ cd dist
$ python -m SimpleHTTPServer 8000
```

Now visit `localhost:8000` in your browser.

# Deployment at Github Pages

To publish the site on Github pages, we use the `git worktree` approach described in
[Deploying your JS App to Github Pages the easy way (or not)](https://medium.com/linagora-engineering/deploying-your-js-app-to-github-pages-the-easy-way-or-not-1ef8c48424b7).

When cd'ing into dist, you enter the gh-pages branch and you can therefore just push the changes as follows:

```
cd dist
git add .
git commit -am '...'
git push origin gh-pages
```
