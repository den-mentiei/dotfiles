function load_keys -d "Loads .ssh keys."
	if status --is-login
		if not ssh-add -l >/dev/null
			for key in (ls ~/.ssh/id* | grep -v pub)
				ssh-add -q $key
			end
		end
	end
end
