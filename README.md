# Calendar

A simple calendar that shows the days of the month and
dates of the week.

Written in [PureScript](https://github.com/purescript/purescript).

## Install 

```bash
npm install
```

### Install watchexec

Download from [the repository](https://github.com/watchexec/watchexec).

Add binary to path:

```bash
mv watchexec ~/.local/bin/
mkdir -p ~/.local/bin/man/man1
mv watchexec.1 ~/.local/bin/man/man1/
```

## Run dev server


### Collect static files

```bash
npx gulp
```

### Build PureScript modules

```bash
npm run build:dev
```

### Run the dev server

```bash
npm run serve
```
