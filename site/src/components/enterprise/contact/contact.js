import React, {useState} from 'react';

import {FormattedMessage, useIntl} from 'react-intl';

import {main, form, getintouch, address} from './contact.module.scss';
import {ActionButton} from '../../core/buttons/action-button';

export const Contact = () => {
  const intl = useIntl();

  const [status, setStatus] = useState(undefined);

  const submitForm = ($event) => {
    $event.preventDefault();

    const form = $event.target;
    const data = new FormData(form);
    const xhr = new XMLHttpRequest();
    xhr.open(form.method, form.action);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onreadystatechange = () => {
      if (xhr.readyState !== XMLHttpRequest.DONE) return;
      if (xhr.status === 200) {
        form.reset();
        setStatus('SUCCESS');
      } else {
        setStatus('ERROR');
      }
    };
    xhr.send(data);
  };

  return (
    <section>
      <main className={main}>
        <form className={form} onSubmit={($event) => submitForm($event)} action="https://formspree.io/xjvaebzk" method="POST">
          <h3 className={getintouch}>
            <FormattedMessage id="enterprise.contact.inquiry" />
          </h3>

          <input required={true} inputMode="text" name="company" placeholder={intl.formatMessage({id: 'enterprise.contact.company'})} />

          <input required={true} inputMode="text" name="name" placeholder={intl.formatMessage({id: 'enterprise.contact.name'})} />

          <input required={true} inputMode="email" name="_replyto" placeholder={intl.formatMessage({id: 'enterprise.contact.email'})} />

          <input inputMode="tel" name="phone" placeholder={intl.formatMessage({id: 'enterprise.contact.phone'})} />

          <input inputMode="tel" name="employees" placeholder={intl.formatMessage({id: 'enterprise.contact.employees'})} />

          <textarea name="message" placeholder={intl.formatMessage({id: 'enterprise.contact.message'})} rows={4} />

          {status === 'SUCCESS' ? (
            <p style={{paddingTop: '0.75rem', marginBottom: '0.45rem', alignSelf: 'center'}}>
              <FormattedMessage id="enterprise.contact.thanks" />
            </p>
          ) : (
            <ActionButton
              type="submit"
              color="primary"
              msgId="common.submit"
              style={{width: 'fit-content', alignSelf: 'center', margin: '1.75rem 0'}}></ActionButton>
          )}

          {status === 'ERROR' && (
            <p style={{paddingTop: '0.75rem', marginBottom: '0.45rem', alignSelf: 'center'}}>
              <FormattedMessage id="enterprise.contact.error" />
            </p>
          )}

          <div>
            <small>
              <FormattedMessage id="enterprise.contact.required" />
            </small>
          </div>

          <p className={address}>
            {`DeckDeckGo
c/o The Hub Zürich Association
Sihlquai 131
8005 Zürich, Switzerland`}
          </p>
        </form>
      </main>
    </section>
  );
};
