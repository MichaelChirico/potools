Þ    3      ´  G   L      h  %   i  :        Ê  g   ^  v   Æ     =  D   Æ       2   '     Z     p  =         Æ  O   ç  Y   7          ª     Ê     æ  3   	      8	  #   Y	  `   }	  ,   Þ	  _   
     k
     
  R   ¢
  U   õ
  K   K  7     L   Ï  &        C  G   V  2     ß   Ñ  
   ±  #   ¼  #   à  
          '   +  %   S  
   y       $   ¤     É     Ú     õ  2       8  Y   V  7   °     è  i   q     Û  r   z     í  Z   	     d       O     )   î  x          "        5  (   U  *   ~  7   ©  9   á  <     o   X  B   È  c     !   o       w   °  f   (  _     G   ï  W   7  H        Ø  @   é  8   *  ÿ   c     c  +   o  .        Ê  &   Ö  1   ý  /   /     _  )   k  .        Ä     Ô     ï                  #      $          1                  
         ,                .   3   /                      +                        (      %          2       "          *                -           '          	   &          !   )      0                "Installing" translations with msgfmt (To quit translating, press 'Esc'; progress will be saved) * Be sure to match message templates. The count of templates (%s, %d, etc.) must match in all languages, as must initial and terminal newlines (\n) * Special characters (like newlines, \n, or tabs, \t) should be written just like that (with an escape) * Whenever templates or escaping is happening in a string, these will be 'highlighted' by carets (^) in the line below * While the count of templates must match, the _order_ can be changed by using e.g. %2$s to mean 'use the second input as a string here' * You can skip a translation by entering nothing (just press RETURN) ** BEGINNING TRANSLATION ** ** Oops! Invalid translation -- %s. Retrying... ** ** PLURAL MESSAGES ** ** SINGULAR MESSAGES ** **Note: a similar message was previously translated as: **
%s File %s: %d start(s) / %d end(s) File: %s
Call: %s
Message: %s%s
How would you translate this message into %s?%s File: %s
Call: %s
Plural message: %s%s
How would you translate this message into %s %s?%s Generating .pot files... Generating en@quot translations Getting R-level messages... Getting src-level messages... Missing (or not on PATH) system requirements %s.
%s No languages provided; finishing No messages to translate; finishing Please help supply some metadata about it. You can check https://l10n.gnome.org/teams/<language> Problematic call:
%s
< File:%s, Line:%s >
%s Reproducing these messages here for your reference since they might still provide some utility. Running message diagnostics... Some helpful reminders: Thanks! Please file an issue on GitHub to get this language recognized permanently These GNU tools are commonly available from the Linux package manager for your system These GNU tools are commonly available, try installing from brew or apt-get These tools are available as an Rtools goodie, check %s Typically, this means the corresponding error messages have been refactored. Unmatched start/end pairs in files: %s independently of n received %d unique templated arguments but there are %d in the original received templates not present in the original: %s received the same set of templates, but in incorrect order (%s vs %s). Recall that you can use %%$N to do redirect, e.g. to swap the order of '%%d %%s' to be translated more naturally, your translation can use '%%1$s %%2$d' when n = 0 when n = 0, 5-20, 25-30, 35-40, ... when n = 0, 5-21, 25-31, 35-41, ... when n = 1 when n = 1, 21, 31, 41, ... when n = 100-102, 200-202, 300-302, ... when n = 11-99, 111-199, 211-299, ... when n = 2 when n = 2-4, 22-24, 32-34, ... when n = 3-10, 103-110, 203-210, ... when n is 0 or 1 when n is at bigger than 1 when n is not 1 Project-Id-Version: potools 0.2.3
Report-Msgid-Bugs-To: 
PO-Revision-Date: 2021-11-06 14:19-0700
Last-Translator: Michael Chirico <michaelchirico4@gmail.com>
Language-Team: ja
Language: ja
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
X-Generator: potools 0.2.3
 msgfmtã§ç¿»è¨³ã"è£ç½®"ä¸­ ï¼éä¸­è¨³ãæ­¢ããããã°ã'Esc'ãæ¼ãã¾ããæ­©ã¿ã¯ä¿å­ãã¾ããï¼ é³åãåããã¾ãããã§ããé³åï¼%sã%d ã¹ãã·ã£ã«æå­ï¼æ¹è¡ã\nãã¿ãã\tï¼ã¯ãã ãã®ããï¼ã¨ã¹ã±ã¼ãæå­ä»ãï¼ã«æ¼ãã¾ãããã§ãã é³åãã¨ã¹ã±ã¼ããæå­ä¾ã«ããã¨ä¸è¡ã«ã­ã£ã¬ããï¼^ï¼ã§'å¼·èª¿'ããã¾ãã é³åãåãããªããã°ãªããªãã®ã«_é _ç°ãªã£ã¦ãããã§ããä¾ãã°ã%2$sã¯'ããã«ç¬¬äºåæå­ä¾ãä½¿ã'ã¨ç¤ºãã¾ãã ç¿»è¨³ãã¹ã­ããããããã«ä½ãæ¼ããªãã§ãããã§ãããã RETURNãæ¼ãã¦ç¶ãã¾ãã ** ç¿»è¨³å§ã¾ãã¾ã ** ** é¿ãéã£ãï¼ç¿»è¨³ãç¡å¹ã§ã -- %sãç¹°ãæ¿ããã¦ã¿ã¾ãããã **  ** è¤æ°ã¡ãã»ã¼ã¸ **  ** åæ°ã¡ãã»ã¼ã¸ ** **æ³¨ï¼ä»¥åä¼¼ã¦ãã¡ãã»ã¼ã¸ããã®ããã«è¨³ãã¾ããï¼**
%s ãã¡ã¤ã«%s: %då§ã¾ã / %dçµãã ãã¡ã¤ã«ï¼%s
å¼ã³åºãï¼%s
ã¡ãã»ã¼ã¸ï¼%s%s
ãã®ã¡ãã»ã¼ã¸ã¯ã©ããã£ã¦%sã«è¨³ãã¾ãï¼%s ãã¡ã¤ã«ï¼%s
å¼ã³åºãï¼%s
è¤æ°ã¡ãã»ã¼ã¸ï¼%s%s
ãã®ã¡ãã»ã¼ã¸ã¯%sã©ããã£ã¦%sã«è¨³ãã¾ãï¼%s .potãã¡ã¤ã«çæä¸­ããã en@quotç¿»è¨³çæä¸­ããã Rå´ã¡ãã»ã¼ã¸ç©ããä¸­ããã srcå´ã¡ãã»ã¼ã¸ç©ããä¸­ããã ã·ã¹ãã å¿è¦ã¯æãããPATHã«ã¯ãªã%s.
%s è¨èªä¸ãããã¾ããã§ãããçµããã¾ãã è¨³ãã¡ãã»ã¼ã¸ã¯ããã¾ãããçµããã¾ãã ã¡ã¿ãã¼ã¿ãå°ãä¸ãã¦ãã ãããhttps://l10n.gnome.org/teams/<language>ãåèã§ãã¾ãã ä¸é½åãªå¼ã³åºãï¼
%s
< ãã¡ã¤ã«ï¼%sãè¡ï¼%s >
%s ã¾ã å½¹ã«ç«ã¤ãããããªãã®ã§ããã«åèã¨ãã¦ã¡ãã»ã¼ã¸ãåãã¾ãã ã¡ãã»ã¼ã¸è¨ºæ­ä¸­ããã åèã«ãªãã¯ããã¨ï¼ ãããã¨ããããã¾ãï¼GitHubã§issueãéãã¨ãã®è¨èªã¯ãã¤ã¾ã§ãæ°ã¥ãè¦ã«ãªãã¾ãã ãã®GNUéå·ã¯ãªããã¯ã¹ããã±ã¼ã¸ç®¡çã·ã¹ãã ã§ä¸è¬æã«ãªããã¯ãã§ã ãã®GNUéå·ã¯ä¸è¬æã«ãªãã¾ããããbrewãapt-getãä½¿ã£ã¦è¦ã¦ãã ãã ãã®éå·ã¯Rtoolsã«å«ã¾ãã¦ã¾ãã%såèãã¦ãã ãã ä¸è¬ãç¸å½ã¨ã©ã¼ã¡ãã»ã¼ã¸ããªãã¡ã¯ã¿ãªã³ã°ãããã¨è¡¨ãã ãã¡ã¤ã«%sã§åãããªãå§ã¾ã/çµããã¤ããããã¾ã nã«é¢ããã é³åå¼æ°ã¯%dåãã¾ãããã©æ¬ã¯%dãããã¾ãã æ¬ã¡ãã»ã¼ã¸ã«ãªãé³åãåãã¾ããï¼%s é³åã¯çµãåãã§ããé ãç°ãªãã¾ãï¼%så¯¾%sï¼ã%%$Nãè»¢éã®ããã«ä½¿ãããã¨è¦ãã¦ãã ãããä¾ãã°ã'%%d %%s'ãããèªç¶ã«è¨³ãããç§»ããããã°ãç¿»è¨³ã«'%%1$s %%2$d'ä½¿ã£ã¦ãå¤§ä¸å¤«ã§ãã n = 0ãªã n = 0ã5-20ã25-30ã35-40ããããªã n = 0ã5-21ã25-31ã35-41ãããããªã n = 1ãªã n = 1ã21ã31ã41ãããããªã n = 100-102ã200-202ã300-302ãããããªã n = 11-99ã111-199ã211-299ãããããªã n = 2ãªã n = 2-4ã22-24ã32-34ãããããªã n = 3-10ã103-110ã203-210ãããããªã nã0ã1ãªã nã1ããå¤§ãããªã nã1ãããªããªã 