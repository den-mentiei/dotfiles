# Allows to get back to work after gwipping.
function gunwip -d "Gets back to earlier left WIP."
	command git show -s --format=%B | grep -q -c "WIP"; and git reset HEAD~1
end
