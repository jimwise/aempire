/*
 *    Copyright (C) 1987, 1988 Chuck Simmons
 * 
 * See the file COPYING, distributed with empire, for restriction
 * and warranty information.
 *
 * $Id: attack.c,v 1.1 2006/02/21 17:33:41 jwise Exp $
 */

/*
 * attack.c -- handle an attack between two pieces.  We do everything from
 * fighting it out between the pieces to notifying the user who won and
 * killing off the losing object.  Somewhere far above, our caller is
 * responsible for actually removing the object from its list and actually
 * updating the player's view of the world.
 * 
 * Find object being attacked.  If it is a city, attacker has 50% chance
 * of taking city.  If successful, give city to attacker.  Otherwise
 * kill attacking piece.  Tell user who won.
 * 
 * If attacking object is not a city, loop.  On each iteration, select one
 * piece to throw a blow.  Damage the opponent by the strength of the blow
 * thrower.  Stop looping when one object has 0 or fewer hits.  Kill off 
 * the dead object.  Tell user who won and how many hits her piece has left,
 * if any.
 */

#include <assert.h>
#include "empire.h"
#include "extern.h"

void	attack (piece_info_t *, long);
static void	attack_city (piece_info_t *, long);
static void	attack_obj (piece_info_t *, long);
static void	describe (piece_info_t *, piece_info_t *, long);
static void	survive (piece_info_t *, long);

void
attack (piece_info_t *att_obj, long loc)
{
	if (map[loc].contents == '*') /* attacking a city? */
		attack_city(att_obj, loc);
	else
		attack_obj(att_obj, loc); /* attacking a piece */
}

static void
attack_city (piece_info_t *att_obj, long loc)
{
	city_info_t *cityp;
	unsigned char att_owner, city_owner;

	cityp = find_city(loc);
	assert(cityp);
	
	att_owner = att_obj->owner;
	city_owner = cityp->owner;

	if (rand_long (2) == 0) { /* attack fails? */
		if (att_owner == USER)
			info("The scum defending the city crushed your attacking blitzkrieger.");
		else if (city_owner == USER)
			info("Your city at %d is under attack.", cityp->loc);
		
		kill_obj(att_obj, loc);
	}
	else
	{
		/* attack succeeded */
		kill_city(cityp);
		cityp->owner = att_owner;
		kill_obj(att_obj, loc);

		if (att_owner == USER)
		{
			info("City at %d has been subjugated!", cityp->loc);
			info("Your army has been dispersed to enforce control.");
			set_prod(cityp);
		}
		else if (city_owner == USER)
			info("City at %d has been lost to the enemy!", cityp->loc);
	}
	/* let city owner see all results */
	if (city_owner != UNOWNED)
		scan(MAP(city_owner), loc);
}

/*
 * Attack a piece other than a city.  The piece could be anyone's.
 * First we have to figure out what is being attacked.
 */

static void
attack_obj (piece_info_t *att_obj, long loc)
{
	piece_info_t *def_obj; /* defender */
	int owner;

	def_obj = find_obj_at_loc(loc);
	assert(def_obj != NULL); /* can't find object to attack? */
	
	if (def_obj->type == SATELLITE)
		return; /* can't attack a satellite */

	while (att_obj->hits > 0 && def_obj->hits > 0)
	{
		if (rand_long (2) == 0) /* defender hits? */
		     att_obj->hits -= piece_attr[def_obj->type].strength;
		else
			def_obj->hits -= piece_attr[att_obj->type].strength;
	}

	if (att_obj->hits > 0)
	{
		/* attacker won */
		describe(att_obj, def_obj, loc);
		owner = def_obj->owner;
		kill_obj(def_obj, loc); /* kill loser */
		survive(att_obj, loc); /* move attacker */
	}
	else
	{
		/* defender won */
		describe(def_obj, att_obj, loc);
		owner = att_obj->owner;
		kill_obj(att_obj, loc);
		survive(def_obj, loc);
	}
	/* show results to first killed */
	scan(MAP(owner), loc);
}

/*
 * Here we look to see if any cargo was killed in the attack.  If
 * a ships contents exceeds its capacity, some of the survivors
 * fall overboard and drown.  We also move the survivor to the given
 * location.
 */

static void
survive (piece_info_t *obj, long loc)
{
	while (obj_capacity (obj) < obj->count)
		kill_obj(obj->cargo, loc);
		
	move_obj(obj, loc);
}

static void
describe (piece_info_t *win_obj, piece_info_t *lose_obj, long loc)
{
	char buf[STRSIZE];
	char buf2[STRSIZE];
	int diff;
	
	*buf = '\0';
	*buf2 = '\0';
	
	if (win_obj->owner != lose_obj->owner)
	{
		if (win_obj->owner == USER)
		{
			user_score += piece_attr[lose_obj->type].build_time;
			
			info("Enemy %s at %d destroyed.", piece_attr[lose_obj->type].name, loc);
			info("Your %s has %d hits left.", piece_attr[win_obj->type].name, win_obj->hits);
				
			diff = win_obj->count - obj_capacity (win_obj);
			if (diff > 0) switch (win_obj->cargo->type)
			{
		 	    case ARMY:
				info("%d armies fell overboard and drowned in the assault.", diff);
				break;
			    case FIGHTER:
				info("%d fighters fell overboard and were lost in the assault.", diff);
				break;
			    default:
				panic("cargo is not fighter or army");
				break;
			}
		}
		else
		{
			comp_score += piece_attr[lose_obj->type].build_time;
			
			info("Your %s at %d destroyed.", piece_attr[lose_obj->type].name, loc);
		}
	}
}
