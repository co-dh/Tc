# Maintainer: dh
pkgname=tc-git
pkgver=r363.bc8926d
pkgrel=1
pkgdesc='Terminal table viewer (TUI) for parquet, CSV, DuckDB, kdb, S3, HuggingFace'
arch=('x86_64')
url='https://github.com/co-dh/Tc'
license=('MIT')
depends=('duckdb' 'fzf' 'bat' 'viu' 'gnuplot' 'trash-cli' 'curl')
makedepends=('git' 'gcc' 'make')
optdepends=('less: file preview fallback'
            'prqlc: PRQL query compilation (install via cargo)')
source=("${pkgname}::git+file://${startdir}")
sha256sums=('SKIP')
options=('!strip')

pkgver() {
  cd "${pkgname}"
  printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short=7 HEAD)"
}

prepare() {
  cd "${pkgname}"
  # Install elan (Lean toolchain manager) locally if not present
  if ! command -v lake &>/dev/null && [ ! -f "$srcdir/elan/bin/lake" ]; then
    curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
      | ELAN_HOME="$srcdir/elan" bash -s -- -y --no-modify-path
  fi
}

build() {
  cd "${pkgname}"
  export PATH="$srcdir/elan/bin:$HOME/.elan/bin:$PATH"
  lake build tc
}

package() {
  cd "${pkgname}"
  # Binary + data dir (sources.duckdb is found relative to exe)
  install -Dm755 .lake/build/bin/tc "$pkgdir/usr/lib/tc/bin/tc"
  install -Dm644 cfg/sources.duckdb "$pkgdir/usr/lib/tc/bin/cfg/sources.duckdb"

  # Symlink into PATH
  install -dm755 "$pkgdir/usr/bin"
  ln -s /usr/lib/tc/bin/tc "$pkgdir/usr/bin/tc"
}
