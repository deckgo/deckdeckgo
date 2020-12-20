import React from 'react';
import Img from 'gatsby-image';

import {FormattedMessage} from 'react-intl';

import styles from './card.module.scss';

export const Card = ({feed}) => {
  return (
    <figure className={styles.figure}>
      <Img style={{width: '100%', height: 'calc(16vw / 16 * 9)'}} fluid={feed.remoteImage.childImageSharp.fluid} alt={feed.data.title} />
    </figure>
  );
};
