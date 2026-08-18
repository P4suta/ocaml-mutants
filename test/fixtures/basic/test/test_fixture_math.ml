let () =
  assert (Fixture_math.add 2 3 = 5);
  assert (Fixture_math.positive 1);
  assert (not (Fixture_math.positive 0));
  assert (Fixture_math.classify 10);
  assert (Fixture_math.below_limit 9);
  assert (not (Fixture_math.below_limit 10));
  assert (Fixture_math.maybe_positive 1 = Some 1);
  assert (Fixture_math.maybe_positive 0 = None);
  assert (Fixture_math.items 1 = [ 1 ]);
  assert (Fixture_math.items 0 = []);
  assert (Fixture_math.guarded 1);
  assert (not (Fixture_math.guarded 0));
  assert (Fixture_math.combine 5 2 = 3);
  assert (Fixture_math.always () = true)
