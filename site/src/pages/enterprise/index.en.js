import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';
import {EnterpriseHero} from '../../components/enterprise/hero/hero';
import {EnterpriseFeatures} from '../../components/enterprise/features/features';
import {Contact} from '../../components/enterprise/contact/contact';

const EnterprisePage = (props) => {
  return (
    <Layout location={props.location} sticky={false} navTheme={'enterprise'}>
      <EnterpriseHero />

      <EnterpriseFeatures lang="en" />

      <Contact />

      <Footer lang="en" action={false} />
    </Layout>
  );
};

export default EnterprisePage;
