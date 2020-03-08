function isMatch(path, patterns) {
    if (!patterns) return false;

    return micromatch.isMatch(path, patterns);
}

function isTmpFile(path) {
    const last = path[path.length - 1];
    return last === '%' || last === '~';
}

function isHiddenFile(path) {
    return /(^|\/)[_.]/.test(path);
}

function isExcludedFile(path, config) {
  if (isTmpFile(path)) return true;
  if (isMatch(path, config.exclude)) return true;
  if (isHiddenFile(path) && !isMatch(path, config.include)) return true;
  return false;
}

exports.isTmpFile = isTmpFile;
exports.isHiddenFile = isHiddenFile;
exports.isExcludedFile = isExcludedFile;
exports.isMatch = isMatch;
