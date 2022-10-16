import React from 'react';
import {Link} from 'gatsby';

import {FormattedMessage} from 'react-intl';

import {links, sectionTitle, social} from './links.module.scss';

export const Links = ({lang, action, display = 'grid'}) => {
  return (
    <div className={`${links} ${action ? 'action' : ''} ${display}`}>
      <div className={social}>
        <a href="https://twitter.com/deckdeckgo" rel="noopener norefferer" aria-label="Twitter">
          <img
            loading="lazy"
            src="/assets/icons/ionicons/twitter.svg"
            aria-hidden="true"
            alt=""
            style={{width: '2rem', padding: '0.45rem'}}
          />
        </a>
        <a href="https://github.com/deckgo/deckdeckgo" rel="noopener noreferrer" aria-label="GitHub">
          <img
            loading="lazy"
            src="/assets/icons/ionicons/github.svg"
            aria-hidden="true"
            alt=""
            style={{width: '2rem', padding: '0.45rem'}}
          />
        </a>
      </div>
    </div>
  );
};
