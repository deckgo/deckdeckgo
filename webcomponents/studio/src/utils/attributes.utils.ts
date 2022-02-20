export const getAttributes = (el: HTMLElement): Record<string, string> => {
  return Array.from(el?.attributes)
    .map((a: Attr) => [a.name, a.value])
    .reduce((acc, attr) => {
      acc[attr[0]] = attr[1];
      return acc;
    }, {});
};
