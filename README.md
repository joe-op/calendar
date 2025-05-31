# Calendar

A simple calendar that shows the days of the month for a
given month and year.

Written in [PureScript](https://github.com/purescript/purescript).

## Install 

```bash
npm install
```

### Install watchexec

To install with [Cargo](https://doc.rust-lang.org/cargo/):

```bash
cargo install watchexec-cli
```

<details>
<summary>Install binary directly</summary>

Download from [the repository](https://github.com/watchexec/watchexec).

Add binary to path:

```bash
cp watchexec /usr/local/bin/
# see `man man`
# this could also be /usr/share
cp watchexec.1 /usr/local/share/man/man1/
```
</details>

## Build

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

### Package

Bundle files in calendar.tar.gz

```bash
bin/distribute
```

### Deploy

See bin/deploy-sample for an example of deploying the bundle as static assets
to your server.
