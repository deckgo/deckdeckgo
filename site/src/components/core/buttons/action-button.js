import React from 'react';

import styles from './button.module.scss';

import {FormattedMessage} from 'react-intl';

export const ActionButton = ({msgId, color, type, style, action}) => {
  return (
    <button type={type} className={styles.link} color={color} style={style} onClick={action}>
      <FormattedMessage id={msgId} />
    </button>
  );
};
