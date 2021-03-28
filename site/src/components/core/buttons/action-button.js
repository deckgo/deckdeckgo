import React from 'react';

import {link} from './button.module.scss';

import {FormattedMessage} from 'react-intl';

export const ActionButton = ({msgId, color, type, style, action}) => {
  return (
    <button type={type} className={link} color={color} style={style} onClick={action}>
      <FormattedMessage id={msgId} />
    </button>
  );
};
