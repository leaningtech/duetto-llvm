//===-- Cheerp/DeterministicUnorderedSet.h - Cheerp utility class ---------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_DETERMINISTIC_UNORDERED_SET_H
#define _CHEERP_DETERMINISTIC_UNORDERED_SET_H

#include "llvm/Cheerp/DeterministicUnorderedImpl.h"

/*
   Deterministic Unordered Map

   Currently implement the most common map-operations (insert, operator[], count, find, erase, clear, empty, swap, operator=)
   with a slight per-operation additional cost over a std::map (roughly 2x in memory size and negligible in time-complexity)
   All operation, iterations comprised, are deterministic

   No operation invalidates "live" iterators (eg. after you call delete, you should not access an iterator)

   The determinism is achieved by keeping 2 different datastructure updated at the same time, a std::map<key,std::list<>::iterator> and a std::list<std::pair<key, mapped>>.
   Iterations are done on the list iterators while the other operations are performed first on the map
*/


namespace cheerp
{

template <typename Key, class Hash_Key, template<typename, typename...> class Container, bool CouldErase>
class DeterministicUnorderedSetImpl : public
	DeterministicUnorderedImpl<Key, Key, Hash_Key, Container, CouldErase>
{
public:
	using value_type = Key;
	using BaseClass = DeterministicUnorderedImpl<Key, value_type, Hash_Key, Container, CouldErase>;
	using BaseClass::BaseClass;
	using typename DeterministicUnorderedImpl<Key, value_type, Hash_Key, Container, CouldErase>::iterator;
	using typename DeterministicUnorderedImpl<Key, value_type, Hash_Key, Container, CouldErase>::const_iterator;
	using BaseClass::map;
	using BaseClass::container;
	std::pair<iterator,bool> emplace(const Key& k)
	{
		return insertImpl(k);
	}
	std::pair<iterator,bool> insertImpl(const Key& k)
	{
		iterator W = this->find(k);
		if (W != BaseClass::end())
			return {W, false};
		container.push_back(k);
		W = --BaseClass::end();
		map[this->mapped(*W)] = W;
		return {W, true};
	}
	std::pair<iterator,bool> insert(const value_type& value)
	{
		return insertImpl(value);
	}
	template<typename Iter>
	DeterministicUnorderedSetImpl(Iter I, Iter E) : DeterministicUnorderedSetImpl()
	{
		insert<Iter>(I, E);
	}
	DeterministicUnorderedSetImpl(const DeterministicUnorderedSetImpl& other) : DeterministicUnorderedSetImpl()
	{
		this->operator=(other);
	}
	DeterministicUnorderedSetImpl& operator=(const DeterministicUnorderedSetImpl& rhs)
	{
		if (this != &rhs)
		{
			this->clear();
			map.reserve(rhs.size());
			insert(rhs.begin(), rhs.end());
		}
		return *this;
	}
	template<typename It>
	void insert(It b, It e)
	{
		while (b != e)
		{
			insert(*b);
			++b;
		}
	}
	template<bool couldErase>
	bool eraseImpl(const Key& t);
	template<>
	bool eraseImpl<true>(const Key& t)
	{
		iterator W = find(t);
		if (W == BaseClass::end())
			return false;
		map.erase(this->mapped(*W));
		BaseClass::removeFrom<BaseClass::ContainerLocal>(W);
		return true;
	}
	template<>
	bool eraseImpl<false>(const Key& t)
	{
		static_assert(CouldErase, "No erase are possible");
		return false;
	}
	bool erase(const Key& t)
	{
		return eraseImpl<CouldErase>(t);
	}
};

template <typename Key, class Hash_Key, bool CouldErase>
class DequeSet : public DeterministicUnorderedSetImpl<Key, Hash_Key, std::deque, CouldErase>
{
	using BaseClass = DeterministicUnorderedSetImpl<Key, Hash_Key, std::deque, CouldErase>;
	using BaseClass::BaseClass;
};

template <typename Key, class Hash_Key, bool CouldErase>
class ListSet : public DeterministicUnorderedSetImpl<Key, Hash_Key, std::list, CouldErase>
{
	using BaseClass = DeterministicUnorderedSetImpl<Key, Hash_Key, std::deque, CouldErase>;
	using BaseClass::BaseClass;
};

template <typename Key, RestrictionsLifted restrictionVoided = RestrictionsLifted::None, class Hash_Key=std::hash<Key>>
class DeterministicUnorderedSet : public std::conditional<
		couldBeDeque<Key>(restrictionVoided),
		DequeSet<Key, Hash_Key, !nonEraseable(restrictionVoided)>,
		ListSet<Key, Hash_Key, !nonEraseable(restrictionVoided)> >::type
{
	using BaseClass = typename std::conditional<
		couldBeDeque<Key>(restrictionVoided),
		DequeSet<Key, Hash_Key, !nonEraseable(restrictionVoided)>,
		ListSet<Key, Hash_Key, !nonEraseable(restrictionVoided)> >::type;
	using BaseClass::BaseClass;
};

}

#endif
