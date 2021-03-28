import React from 'react';
import {GatsbyImage} from 'gatsby-plugin-image';

import {figure} from './card.module.scss';

export const Card = ({feed}) => {
  return (
    <a href={feed.data.url} rel="noopener noreferrer" aria-label={feed.data.title}>
      <figure className={figure}>
        <GatsbyImage image={feed.remoteImage.childImageSharp.gatsbyImageData} alt={feed.data.title} />
      </figure>
    </a>
  );
};
