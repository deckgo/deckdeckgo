import React, {useEffect, useState} from 'react';

import {useIntl} from 'react-intl';

import {article, close, link} from './announcement.module.scss';

export const Announcement = () => {
  const intl = useIntl();

  const [display, setDisplay] = useState(false);

  const key = 'deckgo-hide-announcement-ic';

  useEffect(() => {
    if (localStorage.getItem(key)) {
      return;
    }

    setDisplay(true);
  }, []);

  const hide = ($event) => {
    $event.preventDefault();

    localStorage.setItem(key, 'true');
    setDisplay(false);
  };

  return renderAnnouncement();

  function renderAnnouncement() {
    if (!display) {
      return <></>;
    }

    return (
      <article className={article}>
        <a
          className={link}
          href="https://medium.com/geekculture/bye-bye-amazon-google-hello-web-3-0-b01bfe8f8783"
          rel="noopener noreferrer">
          We are porting DeckDeckGo to DFINITY’s Internet Computer.
        </a>

        <button className={close} aria-label={intl.formatMessage({id: 'nav.close.announcement'})} onClick={hide}>
          <svg
            xmlns="http://www.w3.org/2000/svg"
            width="512"
            height="512"
            viewBox="0 0 512 512"
            style={{width: '2rem', padding: '0.45rem', color: 'var(--color-secondary-contrast)'}}>
            <path
              fill="currentColor"
              d="M289.94,256l95-95A24,24,0,0,0,351,127l-95,95-95-95A24,24,0,0,0,127,161l95,95-95,95A24,24,0,1,0,161,385l95-95,95,95A24,24,0,0,0,385,351Z"
            />
          </svg>
        </button>
      </article>
    );
  }
};
