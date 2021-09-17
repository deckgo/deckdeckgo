# Development

Beside the [infra](https://github.com/deckgo/deckdeckgo/tree/main/infra) and code related to the [Internet Computer](https://dfinity.org/), this mono-repo is a collection a JavaScript, more precisely TypeScript project. To run or develop locally part it proceed as following:

### Getting Started

We are using `npm` **>= v7**.

### Web Components, Templates, Providers and Utils

The [webcomponents](https://github.com/deckgo/deckdeckgo/tree/main/webcomponents), [templates](https://github.com/deckgo/deckdeckgo/tree/main/templates), [providers](https://github.com/deckgo/deckdeckgo/tree/main/providers) and [utils](https://github.com/deckgo/deckdeckgo/tree/main/templates) are npm [workspaces](https://docs.npmjs.com/cli/v7/using-npm/workspaces/).

To get the repo and install the dependencies run:

```
git clone https://github.com/deckgo/deckdeckgo
cd deckdeckgo
npm ci
```

To run one of these particular projects, from root, run:

```
npm run start --workspace=webcomponents/core
npm run start --workspace=templates/slide-title
npm run start --workspace=docs
npm run build --workspace=utils/kit
```

### Apps, Docs, Backend and other projects

The other projects are not part of the workspaces. Reason behind is that for production we want to rely on pined and published dependencies and not those build locally as npm v7 create simlinks.

To get the repo, install the dependencies and run an apps, proceed as following:

```
git clone https://github.com/deckgo/deckdeckgo
cd deckdeckgo/studio
npm ci
npm run start
```
