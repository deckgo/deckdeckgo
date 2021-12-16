export const convertStyle = (originalStyle: string | undefined): Record<string, string | undefined> | undefined => {
  if (!originalStyle || originalStyle.length <= 0) {
    return undefined;
  }

  const result: Record<string, string | undefined> = {};

  const styles: string[] = originalStyle.split(';');

  styles.forEach((style: string) => {
    if (style && style.length > 0) {
      const split: string[] = style.split(':');
      if (split && split.length > 1) {
        result[split[0].trim()] = split[1].trim();
      } else if (split && split.length > 0) {
        result[split[0].trim()] = undefined;
      }
    }
  });

  return result;
};
