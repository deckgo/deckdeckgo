import React from 'react';

import styles from './button.module.scss';

import {FormattedMessage} from 'react-intl';

export const LinkButton = ({msgId, targetUrl, color, style}) => {
  return (
    <a href={targetUrl} rel="noopener noreferrer" className={styles.link} color={color} style={style}>
      <FormattedMessage id={msgId} />
    </a>
  );
};
