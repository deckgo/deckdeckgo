export function getPkgVersion() {
  const pkg = require("../package.json");
  return pkg.version;
}
