export const i18nFormat = (text: string, params: {regex: RegExp; value: string}[]) => {
  params.forEach((param) => (text = text.replace(param.regex, param.value)));

  return text;
};
