import React from 'react';

import { FormattedMessage } from "react-intl";

import { main } from "./try.module.scss";

export const Try = () => {

  return <section>
    <main className={main}>
      <p>
        <FormattedMessage
          id="pricing.try.text"
          values={{
            tryLink: (
              <a
                href="https://app.deckdeckgo.com/editor"
                rel="noopener noreferrer"
                style={{textDecoration: 'underline'}}>
                <FormattedMessage id="pricing.try.link" />
              </a>
            ),
          }}
        />
      </p>
    </main>
  </section>

}
