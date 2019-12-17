module TestModule1

   export test1

   function test1()
      return 1
   end

   module SubModule1
      export test2

      function test2()
         return 2
      end
   end
end
