import React from 'react';

import {link} from './button.module.scss';

import {FormattedMessage} from 'react-intl';

export const LinkButton = ({msgId, targetUrl, color, style}) => {
  return (
    <a href={targetUrl} rel="noopener noreferrer" className={link} color={color} style={style}>
      <FormattedMessage id={msgId} />
    </a>
  );
};
