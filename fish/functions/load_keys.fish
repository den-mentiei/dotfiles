function load_keys -d "Loads .ssh keys."
	if status --is-login
		for key in (ls ~/.ssh/id* | grep -v pub)
			ssh-add -q $key
		end
	end
end
