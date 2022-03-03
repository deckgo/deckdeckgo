export const encodeFilename = ({name}: File): string => encodeURI(name.toLowerCase().replace(/\s/g, '-'));
