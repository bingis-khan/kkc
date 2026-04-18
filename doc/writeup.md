- env escaping (check `mkSchemeForFunction` in parser.zig)
	- when an env escapes, we need to add the context from the function to the escaping env, because it might need the stuff.
		I can proceed in two ways:
			- add self-referential matches (I couldn't handle self references, although it should not be too hard.
				- the match is added to the EnvRef type which holds a reference to Env. Env remains unchanged.
				- in practice i encountered weird bugs ig. when I write about it, it sounds like it shouldn't be THAT hard - 
					we just make a stack linked list of matches. the thing is, matches don't have an ID, so we can't compare a match to a match.
					if we added them, would it be better?).
					- I think there was also a problem of changing the match at the same time.
					   (since sometimes we want to make a copy and sometimes not? (IN WHAT CASES? 
					   		i added heap-allocated assocrefs, because we need to change them at the same time when returning envs with the context
					   		and i basically had to change only one place. maybe I should establish when an assoc gets explicitly copied?))
			- env should not directly depend on itself, so we make a specialized scheme just for this env.
				- we get tvars/envs/assocs from the env which relate to the current function.
			  (IN CASE FOR RECURSION, IT SHOULD NOT COMPILE!
					if the recursive function is in the returned env, then the returned env is in the returned env, which makes the size infinite.
				(i chose this option)
				- this is good for SINGLE ENV IMPLEMENTATION, because we can then check if the envs come from the same instantiation.
				- like self-ref matches, the match is added to the EnvRef type which holds a reference to Env. Env remains unchanged.
				- AT MAKING THE CURRENT FUNCTION SCHEME: 
						- when I detect an escaped env, I gather the tvars/envs/assocs used in the env (checks in INSTANTIATED TYPES, but also...).
						- I must also traverse UNINSTANTIATED types of a function (including its env) if the function is INSIDE the CURRENT FUNCTION.
						- this seems like more work. I guess it's more work in narrow cases, but still. maybe an ID system for Matches would be better?
							or would pointer equality be better? I might add self-referential envs in the future as opposed to the current strategy.
							also, it handles function-call-function cases automatically.
	- currently unhandled: variables from env. what happens if we assign a function which is polymorphic / is from a polymorphic function
	  and gets assigned to a variable external to all that polymorphicity? do we use the same strategy? is it considered part of the scheme?

