package com.unper.samper.model;

import lombok.*;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.*;

import com.unper.samper.model.common.Audit;
import com.unper.samper.model.constant.ERole;



@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "role", schema = "public")
public class Role extends Audit{
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Enumerated(EnumType.STRING)
    private ERole name;

    @ManyToMany(mappedBy = "roles")
    private Set<User> users = new HashSet<>();
    
}