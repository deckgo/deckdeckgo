import {Fragment, h, JSX} from '@stencil/core';

export const i18nFormat = (text: string, params: {placeholder: string; value: string | JSX.IntrinsicElements}[]): string => {
  params.forEach((param) => {
    const split = text.split(param.placeholder);
    text = split[0] + param.value + (split.length > 1 ? split[1] : '');
  });

  return text;
};

export const renderI18n = (text: string, param: {placeholder: string; value: string | JSX.IntrinsicElements}): string => {
  const split = text.split(param.placeholder);
  return (
    <Fragment>
      <span>{split[0]}</span>
      {param.value}
      {split.length > 1 ? <span>{split[1]}</span> : undefined}
    </Fragment>
  );
};
