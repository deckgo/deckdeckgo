import React from 'react';

import {FormattedMessage, useIntl} from 'react-intl';

import styles from './feature.module.scss';

export const Feature = ({titleKey, msgKey, enterprise}) => {
  const intl = useIntl();

  return (
    <article>
      <h3>
        <FormattedMessage id={titleKey} />{' '}
        {enterprise ? (
          <img
            loading="lazy"
            src="/assets/icons/ionicons/business.svg"
            alt={intl.formatMessage({id: 'enterprise.features.enterprise.only'})}
            className={styles.enterprise}
          />
        ) : undefined}
      </h3>

      <p>
        <FormattedMessage id={msgKey} />
      </p>
    </article>
  );
};
