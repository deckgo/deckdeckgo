// Excalidraw uses Map

// https://stackoverflow.com/a/56150320/5404186
export const jsonMapReplacer = (_key, value: any): any => {
  if (value instanceof Map) {
    return {
      dataType: 'Map',
      value: Array.from(value.entries()) // or with spread: value: [...value]
    };
  }

  return value;
};

export const jsonMapReviver = (_key, value: any): any => {
  if (typeof value === 'object' && value !== null) {
    if (value.dataType === 'Map') {
      return new Map(value.value);
    }
  }

  return value;
};
