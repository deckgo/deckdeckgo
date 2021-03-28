import * as React from 'react';

import Layout from '../components/core/layout/en';

import {Footer} from '../components/core/footer/footer';

import {main} from '../themes/templates/template.module.scss';

const TermsPage = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <main className={main}>
          <h1>Terms and Conditions of Use</h1>

          <h2>1. Terms</h2>

          <p>
            By accessing and using this platform, you are agreeing to be bound by these Terms and Conditions of Use, our Privacy Policy, all applicable laws and
            regulations, and agree that you are responsible for compliance with any applicable local laws. If you do not agree with any of these terms, you are
            prohibited from using or accessing this platform. The materials contained in this platform and web site are protected by applicable copyright and
            trade mark law.
          </p>

          <h2>2. Use License</h2>

          <p>
            a. Permission is granted to temporarily download one copy of the materials (information or software) on DeckDeckGo's web site and platform for
            personal or commercial viewing only. This is the grant of a license, not a transfer of title, and under this license you may not:
          </p>

          <ul>
            <li>modify or copy the materials;</li>
            <li>attempt to decompile or reverse engineer any software contained on DeckDeckGo's web site;</li>
            <li>remove any copyright or other proprietary notations from the materials; or</li>
            <li>transfer the materials to another person or "mirror" the materials on any other server.</li>
          </ul>

          <p>
            b. This license shall automatically terminate if you violate any of these restrictions and may be terminated by DeckDeckGo at any time. Upon
            terminating your viewing of these materials or upon the termination of this license, you must destroy any downloaded materials in your possession
            whether in electronic or printed format.
          </p>

          <h2>3. Disclaimer</h2>

          <p>
            The materials on DeckDeckGo's web site are provided "as is". DeckDeckGo makes no warranties, expressed or implied, and hereby disclaims and negates
            all other warranties, including without limitation, implied warranties or conditions of merchantability, fitness for a particular purpose, or
            non-infringement of intellectual property or other violation of rights. Further, DeckDeckGo does not warrant or make any representations concerning
            the accuracy, likely results, or reliability of the use of the materials on its Internet web site or otherwise relating to such materials or on any
            sites linked to this site.
          </p>

          <h2>4. Limitations</h2>

          <p>
            In no event shall DeckDeckGo or its suppliers be liable for any damages (including, without limitation, damages for loss of data or profit, or due
            to business interruption,) arising out of the use or inability to use the materials on DeckDeckGo's Internet site, even if DeckDeckGo or an
            authorized representative has been notified orally or in writing of the possibility of such damage. Because some jurisdictions do not allow
            limitations on implied warranties, or limitations of liability for consequential or incidental damages, these limitations may not apply to you.
          </p>

          <h2>5. Revisions and Errata</h2>

          <p>
            The materials appearing on DeckDeckGo's web site or platform could include technical, typographical, or photographic errors. DeckDeckGo does not
            warrant that any of the materials on its web site are accurate, complete, or current. DeckDeckGo may make changes to the materials contained on its
            web site at any time without notice. DeckDeckGo does not, however, make any commitment to update the materials.
          </p>

          <h2>6. Links</h2>

          <p>
            DeckDeckGo has not reviewed all of the sites linked to its Internet web site and is not responsible for the contents of any such linked site. The
            inclusion of any link does not imply endorsement by DeckDeckGo of the site. Use of any such linked web site is at the user's own risk.
          </p>

          <h2>7. Copyright / Takedown</h2>

          <p>
            Users agree and certify that they have rights to share all content that they post on deckdeckgo.com — including, but not limited to, information
            posted in articles, discussions, and comments. This rule applies to prose, code snippets, collections of links, etc. Regardless of citation, users
            may not post copy and pasted content that does not belong to them. Users assume all risk for the content they post, including someone else's
            reliance on its accuracy, claims relating to intellectual property, or other legal rights. If you believe that a user has plagiarized content,
            misrepresented their identity, misappropriated work, or otherwise run afoul of DMCA regulations, please email hello@deckdeckgo.com. DeckDeckGo may
            remove any content users post for any reason.
          </p>

          <h2>8. Site Terms of Use Modifications</h2>

          <p>
            DeckDeckGo may revise these terms of use for its web site or platform at any time without notice. By using this web site or platform you are
            agreeing to be bound by the then current version of these Terms and Conditions of Use.
          </p>

          <h2>9. DeckDeckGo Trademarks and Logos Policy</h2>

          <p>
            All uses of the DeckDeckGo logo, DeckDeckGo badges, brand slogans, iconography, and the like, may only be used with express permission from
            DeckDeckGo. DeckDeckGo reserves all rights, even if certain assets are included in DeckDeckGo open source projects. Please contact
            hello@deckdeckgo.com with any questions or to request permission.
          </p>

          <h2>10. Reserved Names</h2>

          <p>
            DeckDeckGo has the right to maintain a list of reserved names which will not be made publicly available. These reserved names may be set aside for
            purposes of proactive trademark protection, avoiding user confusion, security measures, or any other reason (or no reason).
          </p>

          <p>
            Additionally, DeckDeckGo reserves the right to change any already-claimed name at its sole discretion. In such cases, DeckDeckGo will make
            reasonable effort to find a suitable alternative and assist with any transition-related concerns.
          </p>

          <h2>11. Content Policy</h2>

          <p>
            Users must make a good-faith effort to share content that is on-topic, of high-quality, and is not designed primarily for the purposes of promotion
            or creating backlinks. Additionally, posts must contain substantial content — they may not merely reference an external link that contains the full
            post. This policy applies to comments, articles, and and all other works shared on the DeckDeckGo platform.
          </p>

          <p>
            DeckDeckGo reserves the right to remove any content that it deems to be in violation of this policy at its sole discretion. Additionally, DeckDeckGo
            reserves the right to restrict any user’s ability to participate on the platform at its sole discretion.
          </p>

          <h2>12. Governing Law</h2>

          <p>
            Any claim relating to DeckDeckGo's web site or platform shall be governed by the laws of Switzerland without regard to its conflict of law
            provisions.
          </p>

          <p>General Terms and Conditions applicable to Use of a Web Site and Platform.</p>
        </main>
      </section>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default TermsPage;
