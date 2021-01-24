export function getPkgVersion() {
  // Relative to dist folder
  const pkg = require('../package.json');
  return pkg.version;
}
