---
title: Project Selection Committee
---

# Project selection committee <a name="project-selection-committee"></a>

Projects will be reviewed by this group of people. <a
href="/join-the-committee.html">Interested in joining us?</a>

<div class="committee-members">
$for(members)$
<div class="member">
  <div class="image">
   <img src="/images/project-selection-committee/$imageName$" /> $if(cofounder)$<span class="co-founder">co-founder</span>$endif$ $if(trustee)$<span class="trustee">trustee</span>$endif$
  </div>
  <div class="member-content">
  <h2> <a href="$link$">$name$</a>
  </h2>
   $body$
  </div>
</div>
$endfor$
</div>
